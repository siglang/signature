#[macro_export]
macro_rules! bind {
    ($($f:expr),* => $r:expr) => {{
        $( $f; )*
        $r
    }};
}

#[macro_export]
macro_rules! replace_all {
    ($s:expr, $($t:expr => $r:expr),*) => {{
        let mut s = String::from($s);
        $( s = s.replace($t, $r); )*
        s
    }};
}
