// https://adventofcode.com/2021/day/2

#include <iostream>
#include <string>

using std::cin;
using std::cout;
using std::endl;

int main() {
    struct Sub {
        int pos, dep, aim;
    } sub1 = {}, sub2 = {};
    std::string cmd;
    int val;

    while (cin >> cmd >> val) {
        if (cmd == "forward") {
            sub1.pos += val;
            sub2.pos += val;
            sub2.dep += val * sub2.aim;
        } else if (cmd == "down") {
            sub1.dep += val;
            sub2.aim += val;
        } else if (cmd == "up") {
            sub1.dep -= val;
            sub2.aim -= val;
        } else {
            cout << cmd;
            exit (-1);
        }
    }

    cout << sub1.pos * sub1.dep << endl;
    cout << sub2.pos * sub2.dep << endl;
}
