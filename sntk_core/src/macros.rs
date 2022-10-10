#[macro_export]
macro_rules! bind {
    ($($f:expr),* => $r:expr) => {{
        $( $f; )*
        $r
    }};
}
