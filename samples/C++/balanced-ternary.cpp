#include <iostream>
#include <string>
#include <climits>
using namespace std;

class BalancedTernary {
protected:
	// Store the value as a reversed string of +, 0 and - characters
	string value;

	// Helper function to change a balanced ternary character to an integer
	int charToInt(char c) const {
		if (c == '0')
			return 0;
		return 44 - c;
	}

	// Helper function to negate a string of ternary characters
	string negate(string s) const {
		for (int i = 0; i < s.length(); ++i) {
			if (s[i] == '+')
				s[i] = '-';
			else if (s[i] == '-')
				s[i] = '+';
		}
		return s;
	}

public:
	// Default constructor
	BalancedTernary() {
		value = "0";
	}

	// Construct from a string
	BalancedTernary(string s) {
		value = string(s.rbegin(), s.rend());
	}

	// Construct from an integer
	BalancedTernary(long long n) {
		if (n == 0) {
			value = "0";
			return;
		}

		bool neg = n < 0;
		if (neg)
			n = -n;

		value = "";
		while (n != 0) {
			int r = n % 3;
			if (r == 0)
				value += "0";
			else if (r == 1)
				value += "+";
			else {
				value += "-";
				++n;
			}

			n /= 3;
		}

		if (neg)
			value = negate(value);
	}

	// Copy constructor
	BalancedTernary(const BalancedTernary &n) {
		value = n.value;
	}

	// Addition operators
	BalancedTernary operator+(BalancedTernary n) const {
		n += *this;
		return n;
	}

	BalancedTernary& operator+=(const BalancedTernary &n) {
		static char *add = "0+-0+-0";
		static char *carry = "--000++";

		int lastNonZero = 0;
		char c = '0';
		for (int i = 0; i < value.length() || i < n.value.length(); ++i) {
			char a = i < value.length() ? value[i] : '0';
			char b = i < n.value.length() ? n.value[i] : '0';

			int sum = charToInt(a) + charToInt(b) + charToInt(c) + 3;
			c = carry[sum];

			if (i < value.length())
				value[i] = add[sum];
			else
				value += add[sum];

			if (add[sum] != '0')
				lastNonZero = i;
		}

		if (c != '0')
			value += c;
		else
			value = value.substr(0, lastNonZero + 1); // Chop off leading zeroes

		return *this;
	}

	// Negation operator
	BalancedTernary operator-() const {
		BalancedTernary result;
		result.value = negate(value);
		return result;
	}

	// Subtraction operators
	BalancedTernary operator-(const BalancedTernary &n) const {
		return operator+(-n);
	}

	BalancedTernary& operator-=(const BalancedTernary &n) {
		return operator+=(-n);
	}

	// Multiplication operators
	BalancedTernary operator*(BalancedTernary n) const {
		n *= *this;
		return n;
	}

	BalancedTernary& operator*=(const BalancedTernary &n) {
		BalancedTernary pos = *this;
		BalancedTernary neg = -pos; // Storing an extra copy to avoid negating repeatedly
		value = "0";

		for (int i = 0; i < n.value.length(); ++i) {
			if (n.value[i] == '+')
				operator+=(pos);
			else if (n.value[i] == '-')
				operator+=(neg);
			pos.value = '0' + pos.value;
			neg.value = '0' + neg.value;
		}

		return *this;
	}

	// Stream output operator
	friend ostream& operator<<(ostream &out, const BalancedTernary &n) {
		out << n.toString();
		return out;
	}

	// Convert to string
	string toString() const {
		return string(value.rbegin(), value.rend());
	}

	// Convert to integer
	long long toInt() const {
		long long result = 0;
		for (long long i = 0, pow = 1; i < value.length(); ++i, pow *= 3)
			result += pow * charToInt(value[i]);
		return result;
	}

	// Convert to integer if possible
	bool tryInt(long long &out) const {
		long long result = 0;
		bool ok = true;

		for (long long i = 0, pow = 1; i < value.length() && ok; ++i, pow *= 3) {
			if (value[i] == '+') {
				ok &= LLONG_MAX - pow >= result; // Clear ok if the result overflows
				result += pow;
			} else if (value[i] == '-') {
				ok &= LLONG_MIN + pow <= result; // Clear ok if the result overflows
				result -= pow;
			}
		}

		if (ok)
			out = result;
		return ok;
	}
};

int main() {
	BalancedTernary a("+-0++0+");
	BalancedTernary b(-436);
	BalancedTernary c("+-++-");

	cout << "a = " << a << " = " << a.toInt() << endl;
	cout << "b = " << b << " = " << b.toInt() << endl;
	cout << "c = " << c << " = " << c.toInt() << endl;

	BalancedTernary d = a * (b - c);

	cout << "a * (b - c) = " << d << " = " << d.toInt() << endl;

	BalancedTernary e("+++++++++++++++++++++++++++++++++++++++++");

	long long n;
	if (e.tryInt(n))
		cout << "e = " << e << " = " << n << endl;
	else
		cout << "e = " << e << " is too big to fit in a long long" << endl;

	return 0;
}
