// https://adventofcode.com/2021/day/1

#include <iostream>
#include <iterator>
#include <vector>

using std::cin;
using std::cout;
using std::endl;

int main() {
    std::istream_iterator<int> start(cin), end;
    std::vector<int> ary(start,end);

    auto count = [&ary](int n) {
        int total = 0;
        for (int i=0; i<n; ++i) {
            if (ary[i] < ary[i+1]) {
                ++total;
            }
        }
        return total;
    };

    cout << count(ary.size()-1) << endl;

    for (int i=0; i<ary.size()-2; ++i) {
        ary[i] += ary[i+1] + ary[i+2];
    }

    cout << count(ary.size()-3) << endl;
}
