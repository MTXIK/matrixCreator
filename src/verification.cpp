#include <iostream>
#include <vector>
#include <sstream>

int main() {
    std::vector<int> A, B;

    std::string line;
    int num;

    std::cout << "Enter elements of array A (separated by spaces): ";
    std::getline(std::cin, line);
    std::istringstream streamA(line);
    while (streamA >> num) {
        A.push_back(num);
        if (A.size() >= 20) break; 
    }

    std::cout << "Enter elements of array B (separated by spaces): ";
    std::getline(std::cin, line);
    std::istringstream streamB(line);
    while (streamB >> num) {
        B.push_back(num);
        if (B.size() >= 20) break; 
    }

    std::vector<std::vector<int> > C(A.size(), std::vector<int>(B.size()));
    for (size_t i = 0; i < A.size(); ++i) {
        for (size_t j = 0; j < B.size(); ++j) {
            if (A[i] != 0) {
                C[i][j] = A[i] * B[j];
            }
            else {
                C[i][j] = 1;
            }
        }
    }
    std::cout << "Resulting matrix Cmn:\n";
    for (size_t i = 0; i < C.size(); ++i) {
        for (size_t j = 0; j < C[i].size(); ++j) {
            std::cout << C[i][j] << ' ';
        }
        std::cout << '\n';
    }

    return 0;
}
