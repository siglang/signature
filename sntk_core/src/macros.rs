#[macro_export]
macro_rules! inline_if {
    ($cond:expr; $then:expr; $else:expr) => {
        if $cond {
            $then
        } else {
            $else
        }
    };
    ($cond:expr; $then:expr) => {
        if $cond {
            $then
        }
    };
}

#[macro_export]
macro_rules! ret {
    ($($f:expr),*; $r:expr) => {{
        $( $f; )*
        $r
    }};
}
