// https://adventofcode.com/2021/day/1

#include <iostream>
#include <vector>

using std::cin;
using std::cout;
using std::endl;
using std::vector;

int main() {
    vector<int> ary;

    auto count = [&ary](int n) {
        int total = 0;
        for (int i=0; i<n; ++i) {
            if (ary[i] < ary[i+1]) {
                ++total;
            }
        }
        return total;
    };

    int depth;
    while (cin >> depth) {
        ary.push_back(depth);
    }

    cout << count(ary.size()-1) << endl;

    for (int i=0; i<ary.size()-2; ++i) {
        ary[i] += ary[i+1] + ary[i+2];
    }

    cout << count(ary.size()-3) << endl;
}
