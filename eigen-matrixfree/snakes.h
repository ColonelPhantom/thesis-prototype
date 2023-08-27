#include <array>
#include <coroutine>
#include <cstdint>
#include <iostream>
#include <fstream>
#include <variant>
#include <vector>
#include <set>

#include <boost/container/small_vector.hpp>

#include "generator.h"

template <int N> class SnakesLadders {
public:
    // State
    class PlayingState {
    public:
        short turn_of;
        std::array<short, N> positions;

        PlayingState() :
            turn_of(0),
            positions({})
        {}
        PlayingState(short t, std::array<short, N> p) :
            turn_of(t),
            positions(p)
        {}
    public:
        bool operator<(const PlayingState& other) const {
            if (turn_of != other.turn_of) {
                return turn_of < other.turn_of;
            }

            for (int i = 0; i < N; i++) {
                if (positions[i] != other.positions[i]) {
                    return positions[i] < other.positions[i];
                }
            }

            return false;
        }
        bool operator==(const PlayingState& other) const {
            if (turn_of != other.turn_of) return false;
            for (int i = 0; i < N; i++) {
                if (positions != other.positions) return false;
            }
            return true;
        }
    };

    using State = std::variant<int, PlayingState>;

    // Functions
    SnakesLadders(int sq, std::string filepath);
    Generator<std::pair<State, float>> successors_coro(const State&) const;
    boost::container::small_vector<std::pair<State, float>, 16> successors_vec(const State&) const;
    template<typename F> void successors_lam(const State&, F callback) const;
    uint64_t encode(const State&) const;
    State decode(uint64_t) const;
    void printState(const State&);
    bool isTerminal(const State&) const;

    // Fields (global)
    int n_squares;
    std::vector<int> transfers;
    const std::vector<std::pair<int, float>> dicevals;
    const std::set<int> valid_targets;
    const std::vector<int> sq_compress;
    const std::vector<int> sq_decompress;

    // Fields (about state)
    const std::vector<State> terminals;
    const uint64_t num_states;
    const State initial;
};

template<int N>
struct SnakeHash
{
    std::size_t operator()(std::variant<int, typename SnakesLadders<N>::PlayingState> const& state) const noexcept {
        if( std::holds_alternative<int>(state)) {
            return std::get<int>(state);
        }
        typename SnakesLadders<N>::PlayingState p = std::get<typename SnakesLadders<N>::PlayingState>(state);
        std::size_t h = p.turn_of;
        for(int i = 0; i < N; i++) {
            h <<= 7;
            h += p.positions[i];
        }
        return h + N;

    }
};

template <int N>
SnakesLadders<N>::SnakesLadders(int sq, std::string filepath) :
    n_squares(sq),
    transfers([sq, filepath](){
        std::vector<int> t;
        for(int i = 0; i <= sq; i++) {
            t.push_back(i);
        }
        std::ifstream input(filepath);
        int from, to;
        while(input >> from >> to) {
            t[from] = to;
        }
        return t;
    }()),
    dicevals {
        std::pair(1, 1.0/6.0),
        std::pair(2, 1.0/6.0),
        std::pair(3, 1.0/6.0),
        std::pair(4, 1.0/6.0),
        std::pair(5, 1.0/6.0),
        std::pair(6, 1.0/6.0),
    },
    valid_targets([sq, this](){
        std::set<int> reachable;
        for(const auto& t : transfers) {
            if(t < sq) reachable.insert(t);
        }
        return reachable;
    }()),
    sq_compress([sq, this](){
        int j = 0;
        std::vector<int> comp;
        for(int i = 0; i < sq; i++) {
            if(valid_targets.contains(i)) comp.push_back(j++);
            else comp.push_back(0);
        }
        return comp;
    }()),
    sq_decompress([sq, this](){
        std::vector<int> dec;
        for(int i = 0; i < sq; i++) {
            if(valid_targets.contains(i)) dec.push_back(i);
        }
        return dec;
    }()),
    terminals([]() {
        std::vector<State> terminals;
        for (int i = 0; i < N; i++) {
          terminals.push_back(i);
        }
        return terminals;
      }()),
    num_states([this]() {
        int sqs = valid_targets.size();
        std::cout << "Game has " << sqs << " valid squares" << std::endl;
        int positions = 1;
        for (int i = 0; i < N; i++) positions *= sqs; // [pos, pos, ...]
        positions *= N; // [player, pos, pos, ...]
        positions += N; // won | [player, pos, pos, ...]
        return positions;
    }()),
    initial(PlayingState())
{
  std::cout << "Initialized snakes and ladders with " << N << " players"
            << std::endl;
};

template <int N>
Generator<std::pair<typename SnakesLadders<N>::State, float>> SnakesLadders<N>::successors_coro(const State& state) const {
    if( std::holds_alternative<int>(state)) {
        // game is already finished
        co_yield {std::pair(state, 1.0)};
        co_return;
    }
    SnakesLadders<N>::PlayingState s = std::get<SnakesLadders<N>::PlayingState>(state);
    for (const auto& [val, prob] : this->dicevals) {
        auto next = s.positions[s.turn_of] + val;
        auto next_pos = s.positions;
        if(next > n_squares) {
            // Do not move if we would go past the end
            co_yield std::pair(SnakesLadders<N>::PlayingState((s.turn_of + 1) % N, next_pos), prob);
        } else {
            // Check for a snake or ladder
            next_pos[s.turn_of] = this->transfers[next];
            if(next_pos[s.turn_of] == this->n_squares) {
                // If we land on the end, we win
                co_yield (std::pair(s.turn_of, prob));
            } else {
                co_yield (std::pair(SnakesLadders<N>::PlayingState((s.turn_of + 1) % N, next_pos), prob));
            }
        }
    }
}

template<int N>
boost::container::small_vector<std::pair<typename SnakesLadders<N>::State, float>, 16> SnakesLadders<N>::successors_vec(const State& state) const {
    if( std::holds_alternative<int>(state)) {
        // game is already finished
        return {std::pair(state, 1.0)};
    }
    boost::container::small_vector<std::pair<State, float>, 16> retval;
    SnakesLadders<N>::PlayingState s = std::get<SnakesLadders<N>::PlayingState>(state);
    for (const auto& [val, prob] : this->dicevals) {
        auto next = s.positions[s.turn_of] + val;
        auto next_pos = s.positions;
        if(next > n_squares) {
            // Do not move if we would go past the end
            retval.push_back(std::pair(SnakesLadders<N>::PlayingState((s.turn_of + 1) % N, next_pos), prob));
        } else {
            // Check for a snake or ladder
            next_pos[s.turn_of] = this->transfers[next];
            if(next_pos[s.turn_of] == this->n_squares) {
                // If we land on the end, we win
                retval.push_back(std::pair(s.turn_of, prob));
            } else {
                retval.push_back(std::pair(SnakesLadders<N>::PlayingState((s.turn_of + 1) % N, next_pos), prob));
            }
        }
    }
    return retval;
}

template<int N>
template<typename F>
void SnakesLadders<N>::successors_lam(const State& state, F callback) const {
    if( std::holds_alternative<int>(state)) {
        // game is already finished
        callback(state, 1.0);
        return;
    }
    SnakesLadders<N>::PlayingState s = std::get<SnakesLadders<N>::PlayingState>(state);
    for (const auto& [val, prob] : this->dicevals) {
        auto next = s.positions[s.turn_of] + val;
        auto next_pos = s.positions;
        if(next > n_squares) {
            // Do not move if we would go past the end
            callback(SnakesLadders<N>::PlayingState((s.turn_of + 1) % N, next_pos), prob);
        } else {
            // Check for a snake or ladder
            next_pos[s.turn_of] = this->transfers[next];
            if(next_pos[s.turn_of] == this->n_squares) {
                // If we land on the end, we win
                callback(s.turn_of, prob);
            } else {
                callback(SnakesLadders<N>::PlayingState((s.turn_of + 1) % N, next_pos), prob);
            }
        }
    }

}


template <int N> uint64_t SnakesLadders<N>::encode(const SnakesLadders<N>::State& s) const {
    if (std::holds_alternative<int>(s)) {
        return std::get<int>(s);
    }
    SnakesLadders<N>::PlayingState state = std::get<SnakesLadders<N>::PlayingState>(s);
    uint64_t hash = state.turn_of;
    uint64_t multiplier = N;
    for(int i = 0; i < N; i++) {
        hash += multiplier * sq_compress[state.positions[i]];
        multiplier *= valid_targets.size();
    }
    return hash + N;
}

template <int N> typename SnakesLadders<N>::State SnakesLadders<N>::decode(uint64_t hash) const {
    if(hash < N) return (int)hash;
    hash -= N;
    short player = hash % N;
    hash /= N;

    std::array<short, N> positions;
    for(int i = 0; i < N; i++) {
        positions[i] = sq_decompress[hash % valid_targets.size()];
        hash /= valid_targets.size();
    }
    return PlayingState(player, positions);
}

template<int N>
void SnakesLadders<N>::printState(const State& s) {
    if (std::holds_alternative<int>(s)) {
        std::cout << "Won by " << std::get<int>(s) << std::endl;
        return;
    }
    SnakesLadders<N>::PlayingState p = std::get<SnakesLadders<N>::PlayingState>(s);
    std::cout << "Playing " << p.turn_of << ":";
    for(int i = 0; i < N; i++) {
        std::cout << " " << p.positions[i];
    }
    std::cout << std::endl;
}

template<int N>
bool SnakesLadders<N>::isTerminal(const State& s) const {
    return std::holds_alternative<int>(s);
}

template<int N>
class SnakesLaddersBigstep : public SnakesLadders<N> {
public:
    // using SnakesLadders<N>::SnakesLadders;
    SnakesLaddersBigstep(int sq, std::string file);

    using State = typename SnakesLadders<N>::State;
    Generator<std::pair<State, float>> successors_coro(const State&) const;
    boost::container::small_vector<std::pair<State, float>, 8192> successors_vec(const State&) const;
    template<typename F> void successors_lam(const State&, F callback) const;
    uint64_t encode(const State&) const;
    State decode(uint64_t) const;
    // void printState(const State&);
    // bool isTerminal(const State&) const;
    const uint64_t num_states;
private:
    bool isElim(const State&) const;
    template<typename F> void successors_lam2(const State&, F, float) const;

};

template<int N>
SnakesLaddersBigstep<N>::SnakesLaddersBigstep(int sq, std::string file) :
        SnakesLadders<N>(sq, file),
        num_states([this]() {
            int sqs = this->valid_targets.size();
            int positions = 1;
            for (int i = 0; i < N; i++) positions *= sqs; // [pos, pos, ...]
            positions += N; // won | [player, pos, pos, ...]
            return positions;
        }())
    {}

template<int N>
Generator<std::pair<typename SnakesLadders<N>::State, float>> SnakesLaddersBigstep<N>::successors_coro(const State& state) const {
    for (const auto &[next, prob] : SnakesLadders<N>::successors_coro(state)) {
        if (isElim(next)) {
            // co_await SnakesLaddersBigstep<N>::successors_coro(next);
            for(const auto &[next2, prob2] : SnakesLaddersBigstep<N>::successors_coro(next)) {
                co_yield std::pair(next2, prob * prob2); // ugh
            }
        } else {
            co_yield std::pair(next, prob);
        }
    }
}

template<int N>
boost::container::small_vector<std::pair<typename SnakesLaddersBigstep<N>::State, float>, 8192> SnakesLaddersBigstep<N>::successors_vec(const State& state) const {
    boost::container::small_vector<std::pair<typename SnakesLaddersBigstep<N>::State, float>, 8192> result;
    for (const auto &[next, prob] : SnakesLadders<N>::successors_vec(state)) {
        if (isElim(next)) {
            auto nexts = SnakesLaddersBigstep<N>::successors_vec(next);
            result.reserve(nexts.size());
            for(const auto &[next2, prob2] : SnakesLaddersBigstep<N>::successors_vec(next)) {
                result.push_back(std::pair(next2, prob * prob2));
            }
        } else {
            result.push_back(std::pair(next, prob));
        }
    }
    // if (!isElim(state)) std::cout << "Found " << result.size() << " successors" << std::endl;
    return result;
}
template<int N> template<typename F> void SnakesLaddersBigstep<N>::successors_lam(const State& state, F callback) const {
    successors_lam2(state, callback, 1.0);
}

template<int N> template<typename F> void SnakesLaddersBigstep<N>::successors_lam2(const State& state, F callback, float selfprob) const {
    SnakesLadders<N>::successors_lam(state, [&](auto next, auto prob){
        if(isElim(next)) {
            SnakesLaddersBigstep<N>::successors_lam2(next, callback, selfprob*prob);
        } else {
            callback(next, selfprob*prob);
        }
    });
}

template<int N>
bool SnakesLaddersBigstep<N>::isElim(const State& state) const {
    if (std::holds_alternative<int>(state)) {
        return false;
    }
    typename SnakesLadders<N>::PlayingState p = std::get<typename SnakesLadders<N>::PlayingState>(state);
    return p.turn_of > 0;
}

template <int N> uint64_t SnakesLaddersBigstep<N>::encode(const SnakesLaddersBigstep<N>::State& s) const {
    if (std::holds_alternative<int>(s)) {
        return std::get<int>(s);
    }
    typename SnakesLaddersBigstep<N>::PlayingState state = std::get<typename SnakesLaddersBigstep<N>::PlayingState>(s);
    uint64_t hash = 0;
    uint64_t multiplier = 1;
    for(int i = 0; i < N; i++) {
        hash += multiplier * SnakesLaddersBigstep<N>::sq_compress[state.positions[i]];
        multiplier *= SnakesLaddersBigstep<N>::valid_targets.size();
    }
    return hash + N;
}

template <int N> typename SnakesLaddersBigstep<N>::State SnakesLaddersBigstep<N>::decode(uint64_t hash) const {
    if(hash < N) return (int)hash;
    hash -= N;
    short player = 0;

    std::array<short, N> positions;
    for(int i = 0; i < N; i++) {
        positions[i] = SnakesLaddersBigstep<N>::sq_decompress[hash % SnakesLadders<N>::valid_targets.size()];
        hash /= SnakesLaddersBigstep<N>::valid_targets.size();
    }
    return typename SnakesLaddersBigstep<N>::PlayingState(player, positions);
}

