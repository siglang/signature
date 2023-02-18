declare println = fn(number[]) -> boolean; // todo

type F = fn(boolean) -> number[];

auto x = fn(a: number, b: number, spread c: number) ->
    fn(string) -> F
{
    return fn(x: string) -> F =>
            fn(y: boolean) -> number[] => c;
};

println(x(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)("foo")(true));
