module project.localhost/example

replace project.localhost/purescript-native/ffi-loader => ./purescript-native

replace project.localhost/purescript-native/output => ./output

go 1.13

require (
	github.com/purescript-native/go-ffi v0.0.0-20191015034244-22b13919279c // indirect
	github.com/purescript-native/go-runtime v0.1.0 // indirect
	project.localhost/purescript-native/ffi-loader v0.0.0-00010101000000-000000000000 // indirect
	project.localhost/purescript-native/output v0.0.0-00010101000000-000000000000 // indirect
)
