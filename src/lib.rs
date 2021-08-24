#![allow(rustdoc::private_intra_doc_links)]
#![deny(
    // Documentation
	// TODO: rustdoc::broken_intra_doc_links,
	// TODO: rustdoc::missing_crate_level_docs,
	// TODO: missing_docs,
	// TODO: clippy::missing_docs_in_private_items,

    // Other
	deprecated_in_future,
	exported_private_dependencies,
	future_incompatible,
	missing_copy_implementations,
	missing_debug_implementations,
	private_in_public,
	rust_2018_compatibility,
	rust_2018_idioms,
	trivial_casts,
	trivial_numeric_casts,
	unsafe_code,
	unstable_features,
	unused_import_braces,
	unused_qualifications,

	// clippy attributes
	clippy::missing_const_for_fn,
	clippy::redundant_pub_crate,
	clippy::use_self
)]
#![cfg_attr(docsrs, feature(doc_cfg), feature(doc_alias))]

// TODO:
// - error types
// - docs
// - logging

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt;
use std::path::Path;
use std::str::FromStr;

pub const HOME: &str = "HOME";

pub const XDG_CACHE_HOME: &str = "XDG_CACHE_HOME";
pub const XDG_CONFIG_HOME: &str = "XDG_CONFIG_HOME";
pub const XDG_DATA_HOME: &str = "XDG_DATA_HOME";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum XdgHome {
	Cache,
	Config,
	Data,
}

impl fmt::Display for XdgHome {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Debug::fmt(&self, f)
	}
}

impl XdgHome {
	pub fn get(&self) -> Option<OsString> {
		match self {
			Self::Cache => xdg_cache_home(),
			Self::Config => xdg_config_home(),
			Self::Data => xdg_data_home(),
		}
	}

	pub fn env(&self) -> Option<OsString> {
		match self {
			Self::Cache => xdg_cache_home_env(),
			Self::Config => xdg_config_home_env(),
			Self::Data => xdg_data_home_env(),
		}
	}

	pub fn default(&self) -> Option<OsString> {
		match self {
			Self::Cache => xdg_cache_home_default(),
			Self::Config => xdg_config_home_default(),
			Self::Data => xdg_data_home_default(),
		}
	}

	pub const fn var_key(&self) -> &'static str {
		match self {
			Self::Cache => XDG_CACHE_HOME,
			Self::Config => XDG_CONFIG_HOME,
			Self::Data => XDG_DATA_HOME,
		}
	}
}

pub fn home() -> Option<OsString> {
	std::env::var_os(HOME)
}

macro_rules! xdg_home {
	( $fn:ident, $env_fn:ident, $default_fn:ident, $env_var: ident, $home_suffix:literal ) => {
		pub fn $env_fn() -> Option<OsString> {
			::std::env::var_os($env_var)
		}

		pub fn $default_fn() -> Option<OsString> {
			home().map(|home| {
				Path::new(&home).join($home_suffix).into_os_string()
			})
		}

		pub fn $fn() -> Option<OsString> {
			$env_fn().or_else($default_fn)
		}
	};
}

xdg_home!(
	xdg_cache_home,
	xdg_cache_home_env,
	xdg_cache_home_default,
	XDG_CACHE_HOME,
	".cache"
);

xdg_home!(
	xdg_config_home,
	xdg_config_home_env,
	xdg_config_home_default,
	XDG_CONFIG_HOME,
	".config"
);

xdg_home!(
	xdg_data_home,
	xdg_data_home_env,
	xdg_data_home_default,
	XDG_DATA_HOME,
	".data"
);

// Constant literal for use in `concat` macro
macro_rules! xdg_dir_prefix {
	() => {
		"XDG_"
	};
}

// Constant literal for use in `concat` macro
macro_rules! xdg_dir_suffix {
	() => {
		"_DIR"
	};
}

macro_rules! xdg_dir_common {
    ( $( $name:ident => $key:literal ),* ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub enum XdgDir {
            $( $name , )*
            Custom(::std::borrow::Cow<'static, str>),
        }

        impl XdgDir {
            const XDG_DIR_PREFIX: &'static str = xdg_dir_prefix!();
            const XDG_DIR_SUFFIX: &'static str = xdg_dir_suffix!();

            pub fn custom<S: Into<::std::borrow::Cow<'static, str>>>(s: S) -> Self {
                Self::custom(s.into())
            }

            pub fn env(&self) -> Option<::std::ffi::OsString> {
                let var_key = self.var_key();
                ::std::env::var_os(var_key.as_ref())
            }

            pub fn var_key(&self) -> ::std::borrow::Cow<'static, str> {
                match self {
                    $( Self::$name => ::std::borrow::Cow::Borrowed(concat!(xdg_dir_prefix!(), $key, xdg_dir_suffix!())), )*
                    Self::Custom(custom) => ::std::borrow::Cow::Owned(format!("{}{}{}", Self::XDG_DIR_PREFIX, custom, Self::XDG_DIR_SUFFIX)),
                }
            }
        }

        impl ::std::fmt::Display for XdgDir {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.write_str(self.var_key().as_ref())
            }
        }

        impl<S> ::std::convert::From<S> for XdgDir
        where
            S: Into<::std::borrow::Cow<'static, str>>,
        {
            fn from(value: S) -> Self {
                let value = value.into();
                match value.as_ref() {
                    $( concat!(xdg_dir_prefix!(), $key, xdg_dir_suffix!()) => Self::$name, )*
                    _ => Self::custom(value),
                }
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct XdgDirParseError;

        impl ::std::fmt::Display for XdgDirParseError {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.write_str("failed to parse input as xdg dir")
            }
        }

        impl ::std::error::Error for XdgDirParseError {}

        impl ::std::str::FromStr for XdgDir {
            type Err = XdgDirParseError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $( concat!(xdg_dir_prefix!(), $key, xdg_dir_suffix!()) => Ok(Self::$name), )*
                    _ => {
                        let custom = s.strip_prefix(Self::XDG_DIR_PREFIX).ok_or(XdgDirParseError)?.strip_suffix(Self::XDG_DIR_SUFFIX).ok_or(XdgDirParseError)?;
                        Ok(Self::Custom(::std::borrow::Cow::Owned(custom.to_owned())))
                    }
                }
            }
        }
    };
}

xdg_dir_common! {
	Desktop => "DESKTOP",
	Documents => "DOCUMENTS",
	Download => "DOWNLOAD",
	Music => "MUSIC",
	Pictures => "PICTURES",
	Publicshare => "PUBLICSHARE",
	Templates => "TEMPLATES",
	Videos => "VIDEOS"
}

// TODO: should probably be OsStr
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XdgDirs<'a> {
	dirs: HashMap<XdgDir, Cow<'a, str>>,
}

pub type XdgDirsOwned = XdgDirs<'static>;

impl Clone for XdgDirsOwned {
	fn clone(&self) -> Self {
		Self { dirs: self.dirs.clone() }
	}
}

impl<'a> XdgDirs<'a> {
	pub fn from_ref(s: &'a str) -> Result<Self, XdgDirsParseError> {
		let mut dirs = HashMap::new();

		for line in s.lines() {
			if line.starts_with(XdgDir::XDG_DIR_PREFIX) {
				let (xdg_dir, value) =
					line.split_once('=').ok_or(XdgDirsParseError)?;
				let xdg_dir = XdgDir::from_str(xdg_dir)?;
				dirs.insert(xdg_dir, Cow::Borrowed(value));
			}
		}

		Ok(Self { dirs })
	}

	pub fn to_owned(&self) -> XdgDirsOwned {
		XdgDirsOwned {
			dirs: self
				.dirs
				.iter()
				.map(|(k, v)| (k.clone(), Cow::Owned(v.to_string())))
				.collect(),
		}
	}

	pub fn contains(&self, dir: &XdgDir) -> bool {
		self.dirs.contains_key(dir)
	}

	pub fn contains_dir<D: Into<XdgDir>>(&self, dir: D) -> bool {
		self.contains(&dir.into())
	}

	pub fn path(&'a self, dir: &XdgDir) -> Option<&'a str> {
		self.dirs.get(dir).map(|path| path.as_ref())
	}

	pub fn path_dir<D: Into<XdgDir>>(&'a self, dir: D) -> Option<&'a str> {
		self.path(&dir.into())
	}

	pub fn path_resolved(&'a self, dir: &XdgDir) -> Option<Cow<'a, str>> {
		let path = self.path(dir)?;
		if let Some(stripped) = path.strip_prefix("$HOME") {
			let home = home()?;
			Some(Cow::Owned(
				format!("{}{}", home.to_str().unwrap(), stripped,),
			))
		} else {
			Some(Cow::Borrowed(path))
		}
	}

	pub fn path_resolved_dir<D: Into<XdgDir>>(
		&'a self,
		dir: D,
	) -> Option<Cow<'a, str>> {
		self.path_resolved(&dir.into())
	}
}

impl XdgDirs<'static> {
	pub fn read() -> Result<Self, std::io::Error> {
		let path = xdg_config_home().ok_or_else(|| {
			std::io::Error::new(
				std::io::ErrorKind::NotFound,
				"xdg_config_home was not found",
			)
		})?;
		let path = Path::new(&path).join("user-dirs.dirs");
		let content = std::fs::read_to_string(path)?;

		Self::from_owned(content).map_err(|_| {
			std::io::Error::new(
				std::io::ErrorKind::InvalidData,
				"failed to read xdg dirs file",
			)
		})
	}

	pub fn from_owned<S: AsRef<str>>(s: S) -> Result<Self, XdgDirsParseError> {
		let mut dirs = HashMap::new();

		for line in s.as_ref().lines() {
			if line.starts_with(XdgDir::XDG_DIR_PREFIX) {
				let (xdg_dir, value) =
					line.split_once('=').ok_or(XdgDirsParseError)?;

				// remove `"`
				// TODO: remove asserts
				assert_eq!(value.as_bytes()[0], b'"');
				assert_eq!(value.as_bytes()[value.len() - 1], b'"');
				let value = &value[1..value.len() - 1];

				let xdg_dir = XdgDir::from_str(xdg_dir)?;
				dirs.insert(xdg_dir, Cow::Owned(value.to_owned()));
			}
		}

		Ok(Self { dirs })
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XdgDirsParseError;

impl fmt::Display for XdgDirsParseError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("failed to parse xdg dirs from input")
	}
}

impl std::error::Error for XdgDirsParseError {}

impl From<XdgDirParseError> for XdgDirsParseError {
	fn from(_: XdgDirParseError) -> Self {
		Self
	}
}

impl FromStr for XdgDirs<'static> {
	type Err = XdgDirsParseError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		Self::from_owned(s)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn xdg_homes() {
		assert!(XdgHome::Config.get().is_some());
		assert!(XdgHome::Data.get().is_some());
		assert!(XdgHome::Cache.get().is_some());
	}

	#[test]
	fn xdg_dirs() {
		let dirs = XdgDirs::read().unwrap();
		println!("{:#?}", dirs);
		println!("{:?}", dirs.path(&XdgDir::Music));
	}
}
