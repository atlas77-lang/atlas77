#[macro_export]
macro_rules! declare_error_type {
    {
        #[error($msg:expr)]
        #[derive($($derive:ident),* $(,)?)]
        $vis:vis enum $type_name:ident {
            $($name:ident($ty:ty),)*
        }
    } => {
        #[derive(thiserror::Error, miette::Diagnostic, Debug $(, $derive)*)]
        #[error($msg)]
        $vis enum $type_name {
            $(
                #[error(transparent)]
                #[diagnostic(transparent)]
                $name(#[from] $ty),
            )*
        }
    }
}

#[macro_export]
macro_rules! declare_warning_type {
    {
        #[warning($msg:expr)]
        #[derive($($derive:ident),* $(,)?)]
        $vis:vis enum $type_name:ident {
            $($name:ident($ty:ty),)*
        }
    } => {
        #[derive(thiserror::Error, miette::Diagnostic, Debug $(, $derive)*)]
        #[error($msg)]
        $vis enum $type_name {
            $(
                #[error(transparent)]
                #[diagnostic(transparent, severity(Warning))]
                $name(#[from] $ty),
            )*
        }
    }
}
