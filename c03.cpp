// https://adventofcode.com/2021/day/3

#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

using std::cin;
using std::cout;
using std::endl;

int main() {
    std::istream_iterator<std::string> start(cin), end;
    std::vector<std::string> ary(start,end);
    const int ndata = ary.size();
    const int nbits = ary[0].size();

    int gamma = 0;
    for (int i=0; i<nbits; ++i) {
        int nz = 0;
        for (auto& s : ary) {
            if (s[i] == '0') ++nz;
        }
        gamma *= 2;
        if (nz * 2 <= ndata) gamma += 1;
    }
    const int epsilon = (1 << nbits) - gamma - 1;
    cout << "Part 1: " << gamma * epsilon << " = " << gamma << " * " << epsilon << endl;

    std::sort(ary.begin(),ary.end());

    auto count = [&ary](int index, int lo, int hi) {
        while (true) {
            int md = (lo + hi) / 2;
            if (md == lo) return hi;
            if (ary[md][index] == '0') {
                lo = md;
            } else {
                hi = md;
            }
        }
    };

    auto rating = [&ary,ndata,nbits,&count](bool isCO2) {
        int value = 0, lo = 0, hi = ndata;
        for (int i=0; i<nbits; ++i) {
            value *= 2;
            if (lo+1 == hi) { // only one number left
                if (ary[lo][i] == '1') value += 1;
                continue;
            }
            int nz = count(i,lo,hi);
            if (((nz - lo) * 2 <= hi - lo) == isCO2) {
                hi = nz;
            } else {
                lo = nz;
                value += 1;
            }
        }
        return value;
    };
    const int oxygen = rating(false);
    const int carbon = rating(true);
    cout << "Part 2: " << oxygen * carbon << " = " << oxygen << " * " << carbon << endl;
}
