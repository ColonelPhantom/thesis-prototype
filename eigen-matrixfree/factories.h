#include <cstdint>
#include <iostream>
#include <fstream>
#include <vector>
#include <array>
#include <bitset>

class Factories {
public:
    using State = uint64_t;

    int N;

    Factories(int n) : N{n} {
        std::ifstream input("factory-weights.txt");
        for(int i=0; i<n; i++) {
            double this_p;
            input >> this_p;
            p.push_back(this_p);
            double this_q;
            input >> this_q;
            q.push_back(this_q);
        }
    };

    template<typename F> void successors_lam(const State&, F callback) const;
    uint64_t encode(const State& state) const { return state; };
    State decode(uint64_t state) const { return state; };
    void printState(const State& state) { std::cout << state << std::endl; };
    bool isTerminal(const State& state) const {
        return state == ((1 << N) - 1);
    };

    std::vector<double> p;
    std::vector<double> q;
    const std::vector<State> terminals = std::vector(1, (1ul << N) - 1ul);

    const uint64_t num_states = 1 << N;
    const State initial = 0;

private:
    template<typename F> void successors_lam_rec(const State&, F callback, int i, double prob, const State next) const;
};

template<typename F>
void Factories::successors_lam(const State& current, F callback) const {
    successors_lam_rec(current, callback, 0, 1.0, 0);
}

template<typename F>
void Factories::successors_lam_rec(const State& current, F callback, int i, double prob, const State next) const {
    if(i == N) {
        callback(next, prob);
        return;
    };
    uint64_t factory = 1 << i;
    if(current & factory) {
        // factory is currently on strike
        successors_lam_rec(current, callback, i+1, prob * (q[i]), next); // factory stops striking
        successors_lam_rec(current, callback, i+1, prob * (1-q[i]), next | factory);
    } else {
        // factory is not on strike
        successors_lam_rec(current, callback, i+1, prob * (1-p[i]), next);
        successors_lam_rec(current, callback, i+1, prob * (p[i]), next | factory); // factory starts striking
    }
}
