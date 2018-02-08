struct Some {
	int i;
	int j;
};

int main() {
	auto a = Some{1, 2};

	auto [i...] = a;

	return 0;
}
