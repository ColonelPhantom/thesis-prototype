#include <chrono>
#include <cstdio>
#include <iomanip>
#include <iostream>
#include <map>
#include <unordered_map>
#include <cassert>

#include <eigen3/Eigen/Core>
#include <eigen3/Eigen/Dense>
#include <eigen3/Eigen/IterativeLinearSolvers>
#include <eigen3/Eigen/src/Core/Matrix.h>
#include <eigen3/unsupported/Eigen/IterativeSolvers>

#ifndef SNAKES_LADDERS_PLAYERS
#define SNAKES_LADDERS_PLAYERS 3
#endif

#include "snakes.h"
#include "factories.h"

template<typename G, typename H>
std::unordered_map<typename G::State, double, H> iterate(G& game, const std::unordered_map<typename G::State, double, H>& horizon) {
    std::unordered_map<typename G::State, double, H> result;
    result.reserve(horizon.size());
    for (const auto &[state, value] : horizon) {
        for (const auto &[next, prob] : game.successors_coro(state)) {
            result[next] += value * prob;
        }
    }
    return result;
}

template<typename G>
std::vector<double> iterate_vec(G& game, const std::vector<double>& horizon) {
    std::vector<double> result(game.num_states, 0.0);
    for (size_t i = 0; i < horizon.size(); i++) {
        auto value = horizon[i];
        if(value == 0.0) continue;
        auto state = game.decode(i);
        // for (const auto &[next, prob] : game.successors_coro(state)) {
        //     // assert(game.decode(game.encode(next)) == next);
        //     result[game.encode(next)] += value * prob;
        // }
        if(game.isTerminal(state)) {
            result[i] += value;
        } else {
            game.successors_lam(state, [&](auto next, auto prob){
                result[game.encode(next)] += value * prob;
            });
        }
    }
    return result;
}

template<typename G>
void iterate_mut(G& game, std::vector<double>& horizon) {
    for(size_t i = 0; i < horizon.size(); i++) {
        auto value = horizon[i];
        horizon[i] = 0.0;
        if(value == 0.0) continue;
        auto state = game.decode(i);
        for (const auto &[next, prob] : game.successors_coro(state)) {
            // assert(game.decode(game.encode(next)) == next);
            horizon[game.encode(next)] += value * prob;
        }
    }
}

template<typename G, typename H>
void dump_state_set(std::unordered_map<typename G::State, double, H> &horizon, int iter) {
    std::cout << "Number of states: " << horizon.size() << std::endl;
    for (const auto& [s, prob] : horizon) {
        std::cout << "STATESET" << iter << ": ";
        if (std::holds_alternative<int>(s)) {
            std::cout << "Won(" << std::get<int>(s) << ")" << std::endl;
            continue;
        }
        SnakesLadders<2>::PlayingState p = std::get<SnakesLadders<2>::PlayingState>(s);
        std::cout << "Playing { turn_of: 0, positions: [";
        bool space = false;
        for(int i = 0; i < 2; i++) {
            if(space) std::cout << ", ";
            space = true;
            std::cout << p.positions[i];
        }
        std::cout << "] }" << std::endl;
    }
}

template<typename G, typename H>
void simulate(G& game, double certainty) {
    std::unordered_map<typename G::State, double, H> horizon;
    horizon[game.initial] = 1.0;

    for(int i = 0; ; i++) {
        auto next = iterate(game, horizon);

        // if(i < 15) dump_state_set<G,H>(next, i);

        double finished = 0.0;
        for(const auto& term : game.terminals) {
            finished += next[term];
        }
        if(finished >= certainty) {
            std::cout << "Done after " <<  i << " iterations\n";
            for(const auto& term : game.terminals) {
                std::cout << next[term] * 100 << "%: ";
                game.printState(term);
            }
            return;
        } else {
            std::cout << "Iteration " << i << ", " << finished*100 << " percent done. " <<
            "Horizon size: " << next.size() << std::endl;
        }
        horizon = std::move(next);
    }
}

template<typename G>
void simulate_vec(G &game, double epsilon) {
    auto starttime = std::chrono::high_resolution_clock::now();
    std::vector<double> horizon(game.num_states, 0.0);
    // std::vector<double> next(game.num_states, 0.0);
    horizon[game.encode(game.initial)] = 1.0;

    for(int i = 0; ; i++) {
        auto next = iterate_vec(game, horizon);

        // if(i < 15) dump_state_set<G,H>(next, i);

        double finished = 0.0;
        for(const auto& term : game.terminals) {
            finished += next[game.encode(term)];
        }
        double rel_err = (1.0-finished) / next[game.encode(game.terminals[0])];
        if(rel_err <= epsilon) {
            auto endtime = std::chrono::high_resolution_clock::now();
            auto time_ms = std::chrono::duration<float, std::milli>(endtime - starttime).count();
            std::cout << "Done after " <<  i << " iterations and " << time_ms << "ms\n";
            for(const auto& term : game.terminals) {
                // std::cout << next[game.encode(term)] * 100 << "%: ";
                double prob = next[game.encode(term)];
                std::cout << prob * 100 << "% (corrected: " <<
                    (prob / finished) * 100 << "%): ";
                game.printState(term);
            }
            return;
        } else {
            std::cout << "Iteration " << i << ",\t" << "relative error: " << rel_err << ",\t"
            << finished*100 << " percent done. " <<
            "\tHorizon size: " << next.size() << std::endl;
        }
        horizon = std::move(next);
        // std::swap(horizon, next);
    }
}

template<typename G>
void simulate_n(G &game, int iters) {
    auto starttime = std::chrono::high_resolution_clock::now();
    std::vector<double> horizon(game.num_states, 0.0);
    // std::vector<double> next(game.num_states, 0.0);
    horizon[game.encode(game.initial)] = 1.0;

    for(int i = 1; i <= iters; i++) {
        auto next = iterate_vec(game, horizon);

        std::cerr << '.';

        // if(i < 15) dump_state_set<G,H>(next, i);
        horizon = std::move(next);
        // std::swap(horizon, next);
    }

    double finished = 0.0;
    for(const auto& term : game.terminals) {
        finished += horizon[game.encode(term)];
    }

    auto endtime = std::chrono::high_resolution_clock::now();
    auto time_ms = std::chrono::duration<float, std::milli>(endtime - starttime).count();

    std::cout << "Done after " << time_ms << "ms\n";
    std::cout << "Final probabilities:" << std::endl;
    for(const auto& term : game.terminals) {
        // std::cout << next[game.encode(term)] * 100 << "%: ";
        double prob = horizon[game.encode(term)];
        std::cout << prob * 100 << "%: ";
        game.printState(term);
    }
    std::cout << "Termination probability: " << finished << std::endl;
    return;
}

template<typename G>
void simulate_mut(G &game, double certainty) {
    std::vector<double> horizon(game.num_states, 0.0);
    horizon[game.encode(game.initial)] = 1.0;

    for(int i = 0; ; i++) {
        iterate_mut(game, horizon);

        double finished = 0.0;
        for(const auto& term : game.terminals) {
            finished += horizon[game.encode(term)];
        }
        if(finished >= certainty) {
            std::cout << "Done after " <<  i << " iterations\n";
            for(const auto& term : game.terminals) {
                double prob = horizon[game.encode(term)];
                std::cout << prob * 100 << "% (corrected: " <<
                    (prob / finished) * 100 << "%): ";
                game.printState(term);
            }
            return;
        } else {
            std::cout << "Iteration " << i << ", " << finished*100 << " percent done. " << std::endl;
        }
    }
}

template<typename G>
class GameMatrix;
using Eigen::SparseMatrix;

namespace Eigen {
    namespace internal {
        template<typename G>
        struct traits<GameMatrix<G>> :  public Eigen::internal::traits<Eigen::SparseMatrix<float>>
        {};
    }
}

template<typename G>
class GameMatrix : public Eigen::EigenBase<GameMatrix<G>> {
public:
    // Required typedefs, constants, and method:
    typedef float Scalar;
    typedef float RealScalar;
    typedef int StorageIndex;
    enum {
    ColsAtCompileTime = Eigen::Dynamic,
    MaxColsAtCompileTime = Eigen::Dynamic,
    IsRowMajor = false
    };

    Eigen::Index rows() const { return game.num_states; }
    Eigen::Index cols() const { return game.num_states; }

    template<typename Rhs>
    Eigen::Product<GameMatrix,Rhs,Eigen::AliasFreeProduct> operator*(const Eigen::MatrixBase<Rhs>& x) const {
    return Eigen::Product<GameMatrix,Rhs,Eigen::AliasFreeProduct>(*this, x.derived());
    }

    // Custom API:
    GameMatrix(G g) : game(g) {}

    G game;
};

// Implementation of GameMatrix * Eigen::DenseVector though a specialization of internal::generic_product_impl:
namespace Eigen {
    namespace internal {

        template<typename G, typename Rhs>
        struct generic_product_impl<GameMatrix<G>, Rhs, SparseShape, DenseShape, GemvProduct> // GEMV stands for matrix-vector
            : generic_product_impl_base<GameMatrix<G>,Rhs,generic_product_impl<GameMatrix<G>,Rhs> >
        {
            typedef typename Product<GameMatrix<G>,Rhs>::Scalar Scalar;

            template<typename Dest>
            static void scaleAndAddTo(Dest& dst, const GameMatrix<G>& lhs, const Rhs& rhs, const Scalar& alpha)
            {
                // This method should implement "dst += alpha * lhs * rhs" inplace,
                // however, for iterative solvers, alpha is always equal to 1, so let's not bother about it.
                assert(alpha==Scalar(1) && "scaling is not implemented");
                EIGEN_ONLY_USED_FOR_DEBUG(alpha);

                std::cerr << '.';

                auto starttime = std::chrono::high_resolution_clock::now();

                // #pragma omp parallel for
                for(Index i=0; i < lhs.cols(); ++i) {
                    auto state = lhs.game.decode(i);

                    lhs.game.successors_lam(state, [&](auto next, auto prob){
                        dst(i) += rhs(lhs.game.encode(next)) * prob;
                    });

                    // rewrite "a*x1 + b*x2 = x3" to "a*x1 + b*x2 - x3 = 0"
                    if(!lhs.game.isTerminal(state)) dst(i) -= rhs(i);
                }

                auto endtime = std::chrono::high_resolution_clock::now();
                auto time_ms = std::chrono::duration<float, std::milli>(endtime - starttime).count();

                std::cout << "Matmul of " << lhs.cols() << "x" << lhs.rows() << " took " << time_ms << "ms." << std::endl;

            }
        };
    }
}

template<typename S, typename G> float matrix_prob(G game) {
    auto starttime = std::chrono::high_resolution_clock::now();

    GameMatrix matrix = GameMatrix(game);
    Eigen::VectorXf results(game.num_states);
    results.setZero();
    results[game.encode(0)] = 1.0; // set win probability after win = 1

    S solver;
    // Eigen::BiCGSTAB<GameMatrix<G>, Eigen::IdentityPreconditioner> solver; // example
    // solver.setMaxIterations(50);
    solver.setTolerance(1e-6);
    solver.compute(matrix);
    Eigen::VectorXf x = solver.solve(results);
    auto endtime = std::chrono::high_resolution_clock::now();
    auto time_ms = std::chrono::duration<float, std::milli>(endtime - starttime).count();

    // std::cout << "SOLVED" << std::endl;
    // for(size_t i = 0; i < game.num_states; i++) {
    //     std::cout << x[i] << "\t";
    //     game.printState(game.decode(i));
    // }

    std::cout << typeid(S).name() << "\n\t" <<
        "#iterations: " << solver.iterations() << "\t" <<
        "time: " << time_ms <<" ms\t" <<
        "est. error: " << solver.error() << "\t" <<
        "winning prob: " << x[game.encode(game.initial)] <<
        std::endl;

    return x[game.encode(game.initial)];
}

int main(int argc, char **argv) {
    std::cout << std::setprecision(10);

    if(argc < 4) {
        std::cout << "usage:\n"
            << argv[0] << " snakes-fixed [squares] [transitions-file] [horizon]\n"
            << argv[0] << " snakes-free [squares] [transitions-file]\n"
            << argv[0] << " factories-fixed [factories] [horizon]\n"
            << argv[0] << " factories-free [factories]\n"
            << "snakes compiled with " << SNAKES_LADDERS_PLAYERS << " players\n";
        exit(0);
    }

    if(strcmp(argv[1], "snakes-fixed") == 0) {
        int squares = atoi(argv[2]);
        char *transitions_file = argv[3];
        int horizon = atoi(argv[4]);
        using Game = SnakesLadders<SNAKES_LADDERS_PLAYERS>;
        Game game = Game(squares, transitions_file);
        simulate_n<Game>(game, horizon);
    } else if (strcmp(argv[1], "snakes-free") == 0) {
        int squares = atoi(argv[2]);
        char *transitions_file = argv[3];
        using Game = SnakesLadders<SNAKES_LADDERS_PLAYERS>;
        Game game = Game(squares, transitions_file);
        matrix_prob<Eigen::BiCGSTAB<GameMatrix<Game>, Eigen::IdentityPreconditioner>>(game);
    } else if (strcmp(argv[1], "snakes-iter") == 0) {
        int squares = atoi(argv[2]);
        char *transitions_file = argv[3];
        using Game = SnakesLadders<SNAKES_LADDERS_PLAYERS>;
        Game game = Game(squares, transitions_file);
        // matrix_prob<Eigen::BiCGSTAB<GameMatrix<Game>, Eigen::IdentityPreconditioner>>(game);
        simulate_vec(game, 1e-6);
    } else if (strcmp(argv[1], "factories-fixed") == 0) {
        int factories = atoi(argv[2]);
        int horizon = atoi(argv[3]);
        Factories f(factories);
        simulate_n<Factories>(f, horizon);
    } else if (strcmp(argv[1], "factories-free") == 0) {
        int factories = atoi(argv[2]);
        Factories f(factories);
        matrix_prob<Eigen::BiCGSTAB<GameMatrix<Factories>, Eigen::IdentityPreconditioner>>(f);
    }

    return 0;
}
