#include <iostream>
using namespace std;

int fact(int n) {
    if (n <= 1) return 1;
    return n * fact(n - 1);
}

int main() {
    int n;
    cin >> n;
    int a[5] = {3, 1, 4, 1, 5};
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        if (a[i] % 2 == 1 && i > 0) sum += a[i] * 8;
    }
    cout << "fact=" << fact(n) << endl;
    cout << "sum=" << sum + 2 * 15 << endl;
    return 0;
}