declare println = fn(number[]) -> boolean;

auto fib = fn(n: number) -> number {
    if (n <= 1) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    };
};

println(fib(10));
