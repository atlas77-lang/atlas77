# Changelog

All notable changes to this project will be documented in this file.

## [0.6.0-dev-2] - 2025-12-09

### Bug Fixes

- The --no-std flag was inverted, and the runtime wouldn't load the std lib ([385776d](https://github.com/atlas77-lang/Atlas77/commit/385776d3ebdac0c880e0bb078e02377c56cd74cb))
- Constructor & Destructor body not being monomorphized ([edc577e](https://github.com/atlas77-lang/Atlas77/commit/edc577ef9d03878dc92d604d69b75c7536bd39be))
- Redundant casts are removed, and now int64 & uint64 can be casted to char ([9e49df7](https://github.com/atlas77-lang/Atlas77/commit/9e49df77a2eb03412c69a643c8730863a5f10250))
- Constructor/Destructor can now have local variables ([1dcc693](https://github.com/atlas77-lang/Atlas77/commit/1dcc69373a49069e1856e12c4f6f8abffcec8516))

### Features

- Result<T, E> is now working ([e342835](https://github.com/atlas77-lang/Atlas77/commit/e3428357dbacc76f9d0d0b78da294fc4123ca193))
- Made it so `T?` is now a syntactic sugar for `Option<T>` ([8381d4c](https://github.com/atlas77-lang/Atlas77/commit/8381d4cbce93176be0bec5a1459438fe93415f95))
- Enums are finally working and stable. ([9d01a04](https://github.com/atlas77-lang/Atlas77/commit/9d01a047e4bb109f700c210a48853cd827537a65))
- Added enums to the language. They get replaced by their uint64 value. You can compare them but not do arithmetic on them ([2c7f101](https://github.com/atlas77-lang/Atlas77/commit/2c7f1012b9dc04fc1143758ef01ae21254404599))
- Added &T and &const T in the compiler. They should be fully working ([995de03](https://github.com/atlas77-lang/Atlas77/commit/995de031ef1d9f74ee6c432cf4adce23df3df852))
- The parser now correctly parses function & method generics ([e2c474b](https://github.com/atlas77-lang/Atlas77/commit/e2c474b2334e96888b54fedb450082bec6932cec))

### Refactor

- Removed completely the Runtime rc. It will now be done through a library as an opt-in feature ([f2751ab](https://github.com/atlas77-lang/Atlas77/commit/f2751ab86554ac082e9f75d366c0a11fece43e38))

### Misc

- The brainfuck interpreter is working in `self/` ([9d482c2](https://github.com/atlas77-lang/Atlas77/commit/9d482c2ddd7123d4ae90179d96c16f36f07be518))

# Changelog

All notable changes to this project will be documented in this file.

## [0.6.0-dev] - 2025-12-08

### Bug Fixes

- Fixed edge cases for multi file error not displaying properly. ([368d6d0](https://github.com/atlas77-lang/Atlas77/commit/368d6d019fcf3c4ee5598a220ae36324438c33fd))
- Fix discrepancies of the standard library (mostly syntax) ([e37d75a](https://github.com/atlas77-lang/Atlas77/commit/e37d75a42ee1a84c4f81263312f233efcc8cd643))
- Fixed an issue in the brainfuck lexer... ([fc0e7db](https://github.com/atlas77-lang/Atlas77/commit/fc0e7db472b701feedaf87c5ac35584946f36857))
- CAST_TO would silently failed when it fails to cast T to string. ([ebc0b91](https://github.com/atlas77-lang/Atlas77/commit/ebc0b91a32b6df62fd39e8e4b207450ed886118a))
- Fixed constructor & destructor having too much arguments because the Codegen Table didn't get cleared after generating a constructor/destructor ([79ca645](https://github.com/atlas77-lang/Atlas77/commit/79ca6457543b0118b4982f348cca840951482d26))
- Added a check for when the constructor call args count don't match the constructor arg definition count ([6f7dcff](https://github.com/atlas77-lang/Atlas77/commit/6f7dcffd6f8b608bb0a1a7da0b532e7bf9544fb9))
- Fix an issue with the JMP instruction & the CALL codegen ([446409d](https://github.com/atlas77-lang/Atlas77/commit/446409d394c475bac3c9b4712cb09359aec74ea9))
- Parser errors not having NameSource<String> for #[source_code] ([a820587](https://github.com/atlas77-lang/Atlas77/commit/a820587f8ea8b3d252b0304de444d7ba961981be))
- Multiple broken .atlas files & updated some std files ([44833b8](https://github.com/atlas77-lang/Atlas77/commit/44833b8886dbe524b2549c727e3f067da4c64f31))
- Issue with the typechecker & some examples ([9536128](https://github.com/atlas77-lang/Atlas77/commit/95361285d970612aef3a27fd59763f4dcd7d48ed))
- Issue in constructor args ([deff7f2](https://github.com/atlas77-lang/Atlas77/commit/deff7f220c36cc68299e70d3b1f187524f82cc81))
- Issue with reference counting for classes ([f19db5e](https://github.com/atlas77-lang/Atlas77/commit/f19db5eaed7aec6ea153c6866c150bfb25a422e9))
- Issue with returning pointer ([13731a1](https://github.com/atlas77-lang/Atlas77/commit/13731a1c9b93fbd314ac3d06345a644905a71408))
- Issue with unary op ([4a2f30b](https://github.com/atlas77-lang/Atlas77/commit/4a2f30b83ce254244fe85944bbf6c46a4479ee51))
- Issue #104 ([f27248e](https://github.com/atlas77-lang/Atlas77/commit/f27248e4ca877997c2f29145ecd524e4e594e5fd))

### Documentation

- Updated the CLI to be more accurate and descriptive ([a68b82a](https://github.com/atlas77-lang/Atlas77/commit/a68b82aff06384ba24bb562e1e4002248979b2bb))
- Added blue_engine README.md ([511a202](https://github.com/atlas77-lang/Atlas77/commit/511a202b41ff23d71b697b3a4ec73a1bf888c902))
- Updated README.md and main.rs ([84fc5e7](https://github.com/atlas77-lang/Atlas77/commit/84fc5e762425dff0c420a8749ca32295e2f356b8))
- CHANGELOG.md ([635036b](https://github.com/atlas77-lang/Atlas77/commit/635036b6fb98399d06d782a49350902ee24c9a90))
- Tried to already prepare string & vector library with classes ([ae6c4ef](https://github.com/atlas77-lang/Atlas77/commit/ae6c4ef1701578a13bfccd0ff84f95fddcf4a426))
- Update CHANGELOG.md ([d3b407e](https://github.com/atlas77-lang/Atlas77/commit/d3b407e40c41f7051b66491027e89e0bd68d553f))
- Removed the doc and put it in atlas77-docs ([2e79547](https://github.com/atlas77-lang/Atlas77/commit/2e7954737800eb6c714343670d93e595b47e048e))
- Added some doc and updated it ([b65dcf5](https://github.com/atlas77-lang/Atlas77/commit/b65dcf53bef943eff4cd212521641c6a963bbfb3))
- Mdbook build ([adbd5a6](https://github.com/atlas77-lang/Atlas77/commit/adbd5a67ade26f4796c5ee46a4788bea1672c979))
- More test ([e66c72a](https://github.com/atlas77-lang/Atlas77/commit/e66c72ad60accb03a4b31c6a36c805005d3c8fe4))
- Update to the docs ([3ac1248](https://github.com/atlas77-lang/Atlas77/commit/3ac12482e283eb77894b2c1d5989d163207ed8a3))
- Added some docs for the standard library ([8a2be67](https://github.com/atlas77-lang/Atlas77/commit/8a2be67d73e5edf63ccce99aecea55081df7cbc1))
- Basic setup for documentation of this project ([929f6a9](https://github.com/atlas77-lang/Atlas77/commit/929f6a94e15a424cb6f21f5bdb2b7b4a3661b90d))

### Features

- Added a `Map<K, V>` using two parallel arrays ([ee53a7e](https://github.com/atlas77-lang/Atlas77/commit/ee53a7e7a59f3cb39d7c20ba6bd207753e506178))
- The compiler pipeline is fully working ([8c42644](https://github.com/atlas77-lang/Atlas77/commit/8c42644f0351fb388edb1de584c8385c1c5809ce))
- Removed LoadArg Instruction (now the Call Instruction does it) ([5031314](https://github.com/atlas77-lang/Atlas77/commit/503131439501cbddb1991fadcd67533ebfe4cb8e))
- Fib(40) is fully supported ([2d17e55](https://github.com/atlas77-lang/Atlas77/commit/2d17e55f1481ee5992c6d9544466c375ac7921f8))
- Added the instructions needed for the fibonacci function ([14f4c6c](https://github.com/atlas77-lang/Atlas77/commit/14f4c6cb171109a28040ca2fb6e118c882bb3abf))
- Changed internal representation of Instr. ([a9eb520](https://github.com/atlas77-lang/Atlas77/commit/a9eb520b6fde2941c5b84eb9a3d519caaa45bf9c))
- `Hello World` is now supported on every part of Atlas77. ([6eceb39](https://github.com/atlas77-lang/Atlas77/commit/6eceb398dfc512e2610215311a1881c2bc49681c))
- Added a working codegen & asm for a default 'Hello, Atlas!' program. ([7e37ef7](https://github.com/atlas77-lang/Atlas77/commit/7e37ef7c2f41d7b34dad6fecc4ac75dccb3cd456))
- Fixed Parser tests ([8d9dad2](https://github.com/atlas77-lang/Atlas77/commit/8d9dad23fb970cf5a7131180784bcc174bbb8a40))
- Added proper warnings for case convention ([7a42a33](https://github.com/atlas77-lang/Atlas77/commit/7a42a330303ac7d7c19d5087a45e6082f20a0caa))
- Added init command to the CLI ([87d3d64](https://github.com/atlas77-lang/Atlas77/commit/87d3d643fd4aa02e6677f60836c9248a169b9b13))
- Fixed cyclic imports! ([aed3e82](https://github.com/atlas77-lang/Atlas77/commit/aed3e829da8edc71a72548aa307baed0df0274e7))
- Multi files are supported but are still unstable ([64d451e](https://github.com/atlas77-lang/Atlas77/commit/64d451ea9411b2ddd148a9226bb42e2db18642a4))
- Still unstable structs generics, but it's getting better ([b9c0db1](https://github.com/atlas77-lang/Atlas77/commit/b9c0db13f9c40ac6df55884cc9bd693e27ca433e))
- Standard Library progress std/option, std/result, std/vector. ([5182755](https://github.com/atlas77-lang/Atlas77/commit/518275598da6f242809ad8f7a67ef78add106082))
- Added simple monomorphization for generics ([9b73b0a](https://github.com/atlas77-lang/Atlas77/commit/9b73b0ab2a25546b3d5cdba76e27a0f32766eb05))
- Working struct Generics ([fa57880](https://github.com/atlas77-lang/Atlas77/commit/fa578808f7f49f69d2e2200934e1567f8271c2f7))
- Parser reverted back to 0.5.x but keeping the improvement of 0.6 ([58eee90](https://github.com/atlas77-lang/Atlas77/commit/58eee90548811709f9471bbe5e02ddaca7195e4d))
- Atlas_codegen should be fixed and atlas_asm has been added. ([df60640](https://github.com/atlas77-lang/Atlas77/commit/df6064072bcbf21ba14d482e69eccba0279a0e4b))
- Lexer & Parser supports the new Syntax ([00f0f36](https://github.com/atlas77-lang/Atlas77/commit/00f0f36cd34619b108c295f542d159e01205787c))
- Updated Result & Array in the standard library to the new syntax ([de8f18c](https://github.com/atlas77-lang/Atlas77/commit/de8f18ca446786243960cc2f22cae264fc057630))
- New VM ISA ([cb2b8ef](https://github.com/atlas77-lang/Atlas77/commit/cb2b8ef72496c6b5bb8bb1f08d78e774bcc1c44f))
- Object + RawObject start ([cea07a4](https://github.com/atlas77-lang/Atlas77/commit/cea07a4f9ecc58bc213ce0fb463e59f0ec7da002))
- Object layout ([acb26b2](https://github.com/atlas77-lang/Atlas77/commit/acb26b259d5e8f0a59c2f14eb733bc5853e4f208))
- Start of the runtime rework ([5acbae0](https://github.com/atlas77-lang/Atlas77/commit/5acbae00b6ce6aa18af9eb59a7033304c1aa8c08))
- Readonly type ([4b7cfb7](https://github.com/atlas77-lang/Atlas77/commit/4b7cfb7e8934f3a4a61ae3742d5472b9a5765ac5))
- QoL on extern functions ([caae571](https://github.com/atlas77-lang/Atlas77/commit/caae5718947b84d9b2c10b6efa78ad99009ec0d8))
- Changed how the tag is handled for VMData ([4a0d4bf](https://github.com/atlas77-lang/Atlas77/commit/4a0d4bf561683ebb92ac2a1a121ad5ba74d093f6))
- Tried stuff in the std ([1f93f47](https://github.com/atlas77-lang/Atlas77/commit/1f93f471132439085ebecb71fce5c6ed195e8c4f))
- Added `T?` up to the typechecker ([7610f43](https://github.com/atlas77-lang/Atlas77/commit/7610f43f95be6f15a13e81ed01eb02a81771a77b))
- Added Box<T> for some testings ([6b0799e](https://github.com/atlas77-lang/Atlas77/commit/6b0799ee9a3a0d5dee487e982b49d864fc367ab7))
- Changed classes internal representation. ([8ca29c4](https://github.com/atlas77-lang/Atlas77/commit/8ca29c4bb4477b526a7e39e62a100bcad0f89814))
- Added working classes ([1bd098b](https://github.com/atlas77-lang/Atlas77/commit/1bd098ba8c29337bb60da19e1587d775e74960c3))
- Classes are fully parsed, lowered and type checked! ([dcbefc4](https://github.com/atlas77-lang/Atlas77/commit/dcbefc4f764bb71a5910bf9ceae1b35c9a41dc69))
- We can parse classes now ([fa6a68b](https://github.com/atlas77-lang/Atlas77/commit/fa6a68b25ff2db779114c3699091999d19500dbe))
- Warnings have been added for wrong cases ([9bffcc3](https://github.com/atlas77-lang/Atlas77/commit/9bffcc3b87b025b4c5f817f39e574553637bf73c))
- Basic generics for external function ([1a9e510](https://github.com/atlas77-lang/Atlas77/commit/1a9e5107ecd40728468f677a6be3cc5792b5214d))
- Improved Runtime by optimizing the VarMap ([1223c83](https://github.com/atlas77-lang/Atlas77/commit/1223c838b0caa13dbafdea8d6d9fca74f67dbdfb))
- Made a small matmul in test.atlas ([4867b57](https://github.com/atlas77-lang/Atlas77/commit/4867b57507e8ba5012405fefaa7649c3da68b8c4))
- VMData.tag is now u8 from u16 ([efd12ae](https://github.com/atlas77-lang/Atlas77/commit/efd12ae966e61e1822571e2bd99ee6134fae892d))
- Added PushBool instruction ([ef65471](https://github.com/atlas77-lang/Atlas77/commit/ef65471e1c43155fb3e284a60a3b11c3a2be2d6c))
- Added a working Reference Counting memory management ([1b8ae06](https://github.com/atlas77-lang/Atlas77/commit/1b8ae06a67b9ffea1e6cd46c4464093948b998ee))
- Lists work. [int64] or [float64] should work ([82ef451](https://github.com/atlas77-lang/Atlas77/commit/82ef451ec87ae1cc332d5d5490eb9e43bc87327b))
- Casting is here with the `as` keyword! ([aca37c9](https://github.com/atlas77-lang/Atlas77/commit/aca37c90ea8c0a18e37be756a288cd7983e25968))
- Added strings ([c39ff5a](https://github.com/atlas77-lang/Atlas77/commit/c39ff5a3e95f5a3dbb7a8a3312f55f1d0df64749))
- Added unary operation in the codegen 💀☠️ ([64b14af](https://github.com/atlas77-lang/Atlas77/commit/64b14af56800e10e39c84cbefcd318bfa45042ec))
- Parser for classes and Static access (i.e. ::) ([f137438](https://github.com/atlas77-lang/Atlas77/commit/f137438c65f0e9fdf501d9a0b1fbfddb6e1579f7))
- Type Inference is working ([dfbb536](https://github.com/atlas77-lang/Atlas77/commit/dfbb536f004635e10b13bb99bf14ae9207aa28fd))

### Miscellaneous Tasks

- Cargo clippy ([bd51868](https://github.com/atlas77-lang/Atlas77/commit/bd5186852c8769c55d26e42f8f5d8dddd874239b))
- Update rand requirement from 0.8.5 to 0.9.0 ([4327b05](https://github.com/atlas77-lang/Atlas77/commit/4327b050002fc20cc86ef167ff80f8597eb66c65))
- Prepare for v0.5.1 (again-again-again) ([08989e9](https://github.com/atlas77-lang/Atlas77/commit/08989e96fe39aeb436ec8f367a070cf7234790e4))
- Prepare for v0.5.1 (again-again) ([8655028](https://github.com/atlas77-lang/Atlas77/commit/8655028cfd64957eb535c1dbfea67aff7818ae1a))
- Prepare for v0.5.1 (again) ([39c3879](https://github.com/atlas77-lang/Atlas77/commit/39c3879b162e2a85a89c7399b7954a01c86beeff))
- Prepare for v0.5.1 ([32221d4](https://github.com/atlas77-lang/Atlas77/commit/32221d4d556787915e0096d3e6e1cbb78d7d558b))
- Rand 0.8.5 -> 0.9.0 ([26e0603](https://github.com/atlas77-lang/Atlas77/commit/26e06038b77abb3478eb60d453c3aba7fb84be05))
- Cleaning a bit ([3ffa049](https://github.com/atlas77-lang/Atlas77/commit/3ffa0496c22c24ac2844c19c712354e6cb137d97))
- Updated Cargo.toml files version ([bd743fb](https://github.com/atlas77-lang/Atlas77/commit/bd743fb287ba7890b93e7fd1a20a0be039569855))
- Added a bit of syntax highlighting for VSCode ([29a46f6](https://github.com/atlas77-lang/Atlas77/commit/29a46f6c4bf84e0fa09d0b502803a1f614b28ca6))
- Redid the file structure so it's more easier to navigate ([356b785](https://github.com/atlas77-lang/Atlas77/commit/356b7857ea564ea02d0504e75d4dc317d8ab185e))

### Refactor

- Stack & calling convention refactor ([1c9c034](https://github.com/atlas77-lang/Atlas77/commit/1c9c0340bea1fcf8785e398167057d0061c049e9))
- Redid the file structure once again for `cargo publish` ([56de771](https://github.com/atlas77-lang/Atlas77/commit/56de77191a7172714eaac554158a08b3d73810cc))
- Swapped the lexer from atlas-core to logos ([825fdbe](https://github.com/atlas77-lang/Atlas77/commit/825fdbe06f7d4a557ffd6b65a1ca1ee5f0f58d6b))
- Atlas-core -> logos for a more efficient lexer ([e4bc5d7](https://github.com/atlas77-lang/Atlas77/commit/e4bc5d7f543b7dc502b7a25ab6059a58932ea20d))
- Change type names `i64` -> `int64` ([090fa4f](https://github.com/atlas77-lang/Atlas77/commit/090fa4fe1119b3473f1132f4a9d6dcf1e2fc69fd))
- Changed file structure for the better ([4c40770](https://github.com/atlas77-lang/Atlas77/commit/4c407708930a9a8ce53994d64a2cb92215095aa4))

### Misc

- Did what clippy wanted ([fa4d097](https://github.com/atlas77-lang/Atlas77/commit/fa4d097d325cfe6549de33b805c9b338b5291599))
- Update the logo for a higher res one ([c82968e](https://github.com/atlas77-lang/Atlas77/commit/c82968e71e4ac487c4583f279b6b5b75e0fd85f2))
- Removed unnecessary files ([8e6f6ff](https://github.com/atlas77-lang/Atlas77/commit/8e6f6ff7cc941405f6695bdce09dc581443f4de2))
- Added blue_engine::error in the library ([9980290](https://github.com/atlas77-lang/Atlas77/commit/9980290f3303e18db130d97c6a811f1710ed7d2c))
- Added more errors and warnings ([06b59fe](https://github.com/atlas77-lang/Atlas77/commit/06b59fed0fe9c04fd3100951811686f5551a0132))
- Added an `unstable` warning on every declaration of T? ([5dfafc5](https://github.com/atlas77-lang/Atlas77/commit/5dfafc5646bf73ef14a469649b2b368309b233f2))
- Removed unnecessary crates ([b911502](https://github.com/atlas77-lang/Atlas77/commit/b911502001855150ecd0c6b77f19360d96adf000))
- Started reworking a tiny bit the codegen & the assembler ([d2f5177](https://github.com/atlas77-lang/Atlas77/commit/d2f51771b10c64bab1914f20b197b85423814052))
- Thoughts on the grammar ([6787955](https://github.com/atlas77-lang/Atlas77/commit/6787955009009f4ebd77ff308e5d9c5c74dca382))
- Thinking about life choices ([bb66ba0](https://github.com/atlas77-lang/Atlas77/commit/bb66ba0f1a10cda871708492eb133758dff7e7cc))
- Start of a correct ClassDescriptor ([33a37fe](https://github.com/atlas77-lang/Atlas77/commit/33a37fef5cffb53002a039a552f0f260d77d5d6d))
- Removed unnecessary print(ln) ([062836b](https://github.com/atlas77-lang/Atlas77/commit/062836b1ad8ade82b3c88495025f72ab620ad8e6))
- Git asked me to commit before pushing again ([bf7cbf8](https://github.com/atlas77-lang/Atlas77/commit/bf7cbf8a7fcdc871dfdf20f8851df32af09c10cc))
- Removed debug types in error messages ([b46aa14](https://github.com/atlas77-lang/Atlas77/commit/b46aa143efbb29ef365b9aad7c41abcd7685f657))
- Added some stuff, nothing fancy, mostly comments ([04f0324](https://github.com/atlas77-lang/Atlas77/commit/04f03247a5987469b3f9eee0c5fead172fa8b136))
- Stuff done, no idea what ([58c6aa2](https://github.com/atlas77-lang/Atlas77/commit/58c6aa20b405bb4c3421198c265687e4ec1aee06))

## [0.5.2] - 2025-02-02

### Bug Fixes

- Issue with returning pointer ([13731a1](https://github.com/atlas77-lang/Atlas77/commit/13731a1c9b93fbd314ac3d06345a644905a71408))
- Issue with unary op ([4a2f30b](https://github.com/atlas77-lang/Atlas77/commit/4a2f30b83ce254244fe85944bbf6c46a4479ee51))
- Issue #104 ([f27248e](https://github.com/atlas77-lang/Atlas77/commit/f27248e4ca877997c2f29145ecd524e4e594e5fd))

### Documentation

- Tried to already prepare string & vector library with classes ([ae6c4ef](https://github.com/atlas77-lang/Atlas77/commit/ae6c4ef1701578a13bfccd0ff84f95fddcf4a426))
- Update CHANGELOG.md ([d3b407e](https://github.com/atlas77-lang/Atlas77/commit/d3b407e40c41f7051b66491027e89e0bd68d553f))
- Removed the doc and put it in atlas77-docs ([2e79547](https://github.com/atlas77-lang/Atlas77/commit/2e7954737800eb6c714343670d93e595b47e048e))
- Added some doc and updated it ([b65dcf5](https://github.com/atlas77-lang/Atlas77/commit/b65dcf53bef943eff4cd212521641c6a963bbfb3))
- Mdbook build ([adbd5a6](https://github.com/atlas77-lang/Atlas77/commit/adbd5a67ade26f4796c5ee46a4788bea1672c979))
- More test ([e66c72a](https://github.com/atlas77-lang/Atlas77/commit/e66c72ad60accb03a4b31c6a36c805005d3c8fe4))
- Update to the docs ([3ac1248](https://github.com/atlas77-lang/Atlas77/commit/3ac12482e283eb77894b2c1d5989d163207ed8a3))
- Added some docs for the standard library ([8a2be67](https://github.com/atlas77-lang/Atlas77/commit/8a2be67d73e5edf63ccce99aecea55081df7cbc1))
- Basic setup for documentation of this project ([929f6a9](https://github.com/atlas77-lang/Atlas77/commit/929f6a94e15a424cb6f21f5bdb2b7b4a3661b90d))

### Features

- Added working classes ([1bd098b](https://github.com/atlas77-lang/Atlas77/commit/1bd098ba8c29337bb60da19e1587d775e74960c3))
- Classes are fully parsed, lowered and type checked! ([dcbefc4](https://github.com/atlas77-lang/Atlas77/commit/dcbefc4f764bb71a5910bf9ceae1b35c9a41dc69))
- We can parse classes now ([fa6a68b](https://github.com/atlas77-lang/Atlas77/commit/fa6a68b25ff2db779114c3699091999d19500dbe))
- Warnings have been added for wrong cases ([9bffcc3](https://github.com/atlas77-lang/Atlas77/commit/9bffcc3b87b025b4c5f817f39e574553637bf73c))
- Basic generics for external function ([1a9e510](https://github.com/atlas77-lang/Atlas77/commit/1a9e5107ecd40728468f677a6be3cc5792b5214d))
- Improved Runtime by optimizing the VarMap ([1223c83](https://github.com/atlas77-lang/Atlas77/commit/1223c838b0caa13dbafdea8d6d9fca74f67dbdfb))
- Made a small matmul in test.atlas ([4867b57](https://github.com/atlas77-lang/Atlas77/commit/4867b57507e8ba5012405fefaa7649c3da68b8c4))
- VMData.tag is now u8 from u16 ([efd12ae](https://github.com/atlas77-lang/Atlas77/commit/efd12ae966e61e1822571e2bd99ee6134fae892d))
- Added PushBool instruction ([ef65471](https://github.com/atlas77-lang/Atlas77/commit/ef65471e1c43155fb3e284a60a3b11c3a2be2d6c))
- Added a working Reference Counting memory management ([1b8ae06](https://github.com/atlas77-lang/Atlas77/commit/1b8ae06a67b9ffea1e6cd46c4464093948b998ee))
- Lists work. [int64] or [float64] should work ([82ef451](https://github.com/atlas77-lang/Atlas77/commit/82ef451ec87ae1cc332d5d5490eb9e43bc87327b))
- Casting is here with the `as` keyword! ([aca37c9](https://github.com/atlas77-lang/Atlas77/commit/aca37c90ea8c0a18e37be756a288cd7983e25968))
- Added strings ([c39ff5a](https://github.com/atlas77-lang/Atlas77/commit/c39ff5a3e95f5a3dbb7a8a3312f55f1d0df64749))
- Added unary operation in the codegen 💀☠️ ([64b14af](https://github.com/atlas77-lang/Atlas77/commit/64b14af56800e10e39c84cbefcd318bfa45042ec))
- Parser for classes and Static access (i.e. ::) ([f137438](https://github.com/atlas77-lang/Atlas77/commit/f137438c65f0e9fdf501d9a0b1fbfddb6e1579f7))
- Type Inference is working ([dfbb536](https://github.com/atlas77-lang/Atlas77/commit/dfbb536f004635e10b13bb99bf14ae9207aa28fd))

### Miscellaneous Tasks

- Cargo clippy ([bd51868](https://github.com/atlas77-lang/Atlas77/commit/bd5186852c8769c55d26e42f8f5d8dddd874239b))
- Update rand requirement from 0.8.5 to 0.9.0 ([4327b05](https://github.com/atlas77-lang/Atlas77/commit/4327b050002fc20cc86ef167ff80f8597eb66c65))
- Prepare for v0.5.1 (again-again-again) ([08989e9](https://github.com/atlas77-lang/Atlas77/commit/08989e96fe39aeb436ec8f367a070cf7234790e4))
- Prepare for v0.5.1 (again-again) ([8655028](https://github.com/atlas77-lang/Atlas77/commit/8655028cfd64957eb535c1dbfea67aff7818ae1a))
- Prepare for v0.5.1 (again) ([39c3879](https://github.com/atlas77-lang/Atlas77/commit/39c3879b162e2a85a89c7399b7954a01c86beeff))
- Prepare for v0.5.1 ([32221d4](https://github.com/atlas77-lang/Atlas77/commit/32221d4d556787915e0096d3e6e1cbb78d7d558b))
- Rand 0.8.5 -> 0.9.0 ([26e0603](https://github.com/atlas77-lang/Atlas77/commit/26e06038b77abb3478eb60d453c3aba7fb84be05))
- Cleaning a bit ([3ffa049](https://github.com/atlas77-lang/Atlas77/commit/3ffa0496c22c24ac2844c19c712354e6cb137d97))
- Updated Cargo.toml files version ([bd743fb](https://github.com/atlas77-lang/Atlas77/commit/bd743fb287ba7890b93e7fd1a20a0be039569855))
- Added a bit of syntax highlighting for VSCode ([29a46f6](https://github.com/atlas77-lang/Atlas77/commit/29a46f6c4bf84e0fa09d0b502803a1f614b28ca6))
- Redid the file structure so it's more easier to navigate ([356b785](https://github.com/atlas77-lang/Atlas77/commit/356b7857ea564ea02d0504e75d4dc317d8ab185e))

### Refactor

- Redid the file structure once again for `cargo publish` ([56de771](https://github.com/atlas77-lang/Atlas77/commit/56de77191a7172714eaac554158a08b3d73810cc))
- Swapped the lexer from atlas-core to logos ([825fdbe](https://github.com/atlas77-lang/Atlas77/commit/825fdbe06f7d4a557ffd6b65a1ca1ee5f0f58d6b))
- Atlas-core -> logos for a more efficient lexer ([e4bc5d7](https://github.com/atlas77-lang/Atlas77/commit/e4bc5d7f543b7dc502b7a25ab6059a58932ea20d))
- Change type names `i64` -> `int64` ([090fa4f](https://github.com/atlas77-lang/Atlas77/commit/090fa4fe1119b3473f1132f4a9d6dcf1e2fc69fd))
- Changed file structure for the better ([4c40770](https://github.com/atlas77-lang/Atlas77/commit/4c407708930a9a8ce53994d64a2cb92215095aa4))

### Misc

- Git asked me to commit before pushing again ([bf7cbf8](https://github.com/atlas77-lang/Atlas77/commit/bf7cbf8a7fcdc871dfdf20f8851df32af09c10cc))
- Removed debug types in error messages ([b46aa14](https://github.com/atlas77-lang/Atlas77/commit/b46aa143efbb29ef365b9aad7c41abcd7685f657))
- Added some stuff, nothing fancy, mostly comments ([04f0324](https://github.com/atlas77-lang/Atlas77/commit/04f03247a5987469b3f9eee0c5fead172fa8b136))
- Stuff done, no idea what ([58c6aa2](https://github.com/atlas77-lang/Atlas77/commit/58c6aa20b405bb4c3421198c265687e4ec1aee06))

## [0.5.1] - 2025-01-29

### Bug Fixes

- Issue with returning pointer ([13731a1](https://github.com/atlas77-lang/Atlas77/commit/13731a1c9b93fbd314ac3d06345a644905a71408))
- Issue with unary op ([4a2f30b](https://github.com/atlas77-lang/Atlas77/commit/4a2f30b83ce254244fe85944bbf6c46a4479ee51))
- Issue #104 ([f27248e](https://github.com/atlas77-lang/Atlas77/commit/f27248e4ca877997c2f29145ecd524e4e594e5fd))

### Documentation

- Removed the doc and put it in atlas77-docs ([2e79547](https://github.com/atlas77-lang/Atlas77/commit/2e7954737800eb6c714343670d93e595b47e048e))
- Added some doc and updated it ([b65dcf5](https://github.com/atlas77-lang/Atlas77/commit/b65dcf53bef943eff4cd212521641c6a963bbfb3))
- Mdbook build ([adbd5a6](https://github.com/atlas77-lang/Atlas77/commit/adbd5a67ade26f4796c5ee46a4788bea1672c979))
- More test ([e66c72a](https://github.com/atlas77-lang/Atlas77/commit/e66c72ad60accb03a4b31c6a36c805005d3c8fe4))
- Update to the docs ([3ac1248](https://github.com/atlas77-lang/Atlas77/commit/3ac12482e283eb77894b2c1d5989d163207ed8a3))
- Added some docs for the standard library ([8a2be67](https://github.com/atlas77-lang/Atlas77/commit/8a2be67d73e5edf63ccce99aecea55081df7cbc1))
- Basic setup for documentation of this project ([929f6a9](https://github.com/atlas77-lang/Atlas77/commit/929f6a94e15a424cb6f21f5bdb2b7b4a3661b90d))

### Features

- Improved Runtime by optimizing the VarMap ([1223c83](https://github.com/atlas77-lang/Atlas77/commit/1223c838b0caa13dbafdea8d6d9fca74f67dbdfb))
- Made a small matmul in test.atlas ([4867b57](https://github.com/atlas77-lang/Atlas77/commit/4867b57507e8ba5012405fefaa7649c3da68b8c4))
- VMData.tag is now u8 from u16 ([efd12ae](https://github.com/atlas77-lang/Atlas77/commit/efd12ae966e61e1822571e2bd99ee6134fae892d))
- Added PushBool instruction ([ef65471](https://github.com/atlas77-lang/Atlas77/commit/ef65471e1c43155fb3e284a60a3b11c3a2be2d6c))
- Added a working Reference Counting memory management ([1b8ae06](https://github.com/atlas77-lang/Atlas77/commit/1b8ae06a67b9ffea1e6cd46c4464093948b998ee))
- Lists work. [int64] or [float64] should work ([82ef451](https://github.com/atlas77-lang/Atlas77/commit/82ef451ec87ae1cc332d5d5490eb9e43bc87327b))
- Casting is here with the `as` keyword! ([aca37c9](https://github.com/atlas77-lang/Atlas77/commit/aca37c90ea8c0a18e37be756a288cd7983e25968))
- Added strings ([c39ff5a](https://github.com/atlas77-lang/Atlas77/commit/c39ff5a3e95f5a3dbb7a8a3312f55f1d0df64749))
- Added unary operation in the codegen 💀☠️ ([64b14af](https://github.com/atlas77-lang/Atlas77/commit/64b14af56800e10e39c84cbefcd318bfa45042ec))
- Parser for classes and Static access (i.e. ::) ([f137438](https://github.com/atlas77-lang/Atlas77/commit/f137438c65f0e9fdf501d9a0b1fbfddb6e1579f7))
- Type Inference is working ([dfbb536](https://github.com/atlas77-lang/Atlas77/commit/dfbb536f004635e10b13bb99bf14ae9207aa28fd))

### Miscellaneous Tasks

- Prepare for v0.5.1 (again-again-again) ([08989e9](https://github.com/atlas77-lang/Atlas77/commit/08989e96fe39aeb436ec8f367a070cf7234790e4))
- Prepare for v0.5.1 (again-again) ([8655028](https://github.com/atlas77-lang/Atlas77/commit/8655028cfd64957eb535c1dbfea67aff7818ae1a))
- Prepare for v0.5.1 (again) ([39c3879](https://github.com/atlas77-lang/Atlas77/commit/39c3879b162e2a85a89c7399b7954a01c86beeff))
- Prepare for v0.5.1 ([32221d4](https://github.com/atlas77-lang/Atlas77/commit/32221d4d556787915e0096d3e6e1cbb78d7d558b))
- Rand 0.8.5 -> 0.9.0 ([26e0603](https://github.com/atlas77-lang/Atlas77/commit/26e06038b77abb3478eb60d453c3aba7fb84be05))
- Cleaning a bit ([3ffa049](https://github.com/atlas77-lang/Atlas77/commit/3ffa0496c22c24ac2844c19c712354e6cb137d97))
- Updated Cargo.toml files version ([bd743fb](https://github.com/atlas77-lang/Atlas77/commit/bd743fb287ba7890b93e7fd1a20a0be039569855))
- Added a bit of syntax highlighting for VSCode ([29a46f6](https://github.com/atlas77-lang/Atlas77/commit/29a46f6c4bf84e0fa09d0b502803a1f614b28ca6))
- Redid the file structure so it's more easier to navigate ([356b785](https://github.com/atlas77-lang/Atlas77/commit/356b7857ea564ea02d0504e75d4dc317d8ab185e))

### Refactor

- Redid the file structure once again for `cargo publish` ([56de771](https://github.com/atlas77-lang/Atlas77/commit/56de77191a7172714eaac554158a08b3d73810cc))
- Swapped the lexer from atlas-core to logos ([825fdbe](https://github.com/atlas77-lang/Atlas77/commit/825fdbe06f7d4a557ffd6b65a1ca1ee5f0f58d6b))
- Atlas-core -> logos for a more efficient lexer ([e4bc5d7](https://github.com/atlas77-lang/Atlas77/commit/e4bc5d7f543b7dc502b7a25ab6059a58932ea20d))
- Change type names `i64` -> `int64` ([090fa4f](https://github.com/atlas77-lang/Atlas77/commit/090fa4fe1119b3473f1132f4a9d6dcf1e2fc69fd))
- Changed file structure for the better ([4c40770](https://github.com/atlas77-lang/Atlas77/commit/4c407708930a9a8ce53994d64a2cb92215095aa4))

### Misc

- Removed debug types in error messages ([b46aa14](https://github.com/atlas77-lang/Atlas77/commit/b46aa143efbb29ef365b9aad7c41abcd7685f657))
- Added some stuff, nothing fancy, mostly comments ([04f0324](https://github.com/atlas77-lang/Atlas77/commit/04f03247a5987469b3f9eee0c5fead172fa8b136))
- Stuff done, no idea what ([58c6aa2](https://github.com/atlas77-lang/Atlas77/commit/58c6aa20b405bb4c3421198c265687e4ec1aee06))

# Changelog

All notable changes to this project will be documented in this file.

## [0.5] - 2025-01-17

### Bug Fixes

- Issue with halt shifting the bytecode by 1 ([2932b8c](https://github.com/atlas77-lang/Atlas77/commit/2932b8cfe1f0989e9abebe21acbe09ac2a12df9e))
- Fixed an issue with typechecking in if condition ([229adea](https://github.com/atlas77-lang/Atlas77/commit/229adeac86c3832c7714242808655b3c8187f96e))
- Args in functions were empty resulting in a null error ([ba02e22](https://github.com/atlas77-lang/Atlas77/commit/ba02e22341d6e4140d91a91e7abd3791cdd832a3))
- Return type is optional now ([8d76ee0](https://github.com/atlas77-lang/Atlas77/commit/8d76ee0aabbe1c47fc5cda0e74f742b5f82289a3))

### Documentation

- Redid the Roadmap section of the README.md ([c051231](https://github.com/atlas77-lang/Atlas77/commit/c051231a544e35edf69dc0daefa160a791f92e60))
- Start of SYNTAX.md & redefined the roadmap ([4e606c1](https://github.com/atlas77-lang/Atlas77/commit/4e606c10a2e9139b0303067688e0399ae3e87afe))
- Added git cliff ([2bb7ae5](https://github.com/atlas77-lang/Atlas77/commit/2bb7ae552505170832ccbb24b1415f44eab355e7))

### Features

- Import `std/io` works, other will follow ([c79515c](https://github.com/atlas77-lang/Atlas77/commit/c79515c11949fc3a756ebb7dd1cca9c047bc2df2))
- Should be feature complete for v0.5 ([300d112](https://github.com/atlas77-lang/Atlas77/commit/300d1129fbe96492a8ed3f20f1018ec36b080acc))
- Type Checker seems to be working farily well ([8fac671](https://github.com/atlas77-lang/Atlas77/commit/8fac671ed3831ebcb75ba2f4bd3874982f3d670a))
- Print & println are working ([92d72b5](https://github.com/atlas77-lang/Atlas77/commit/92d72b53567c7e6de84ed5eac137e9551175d423))
- While, if/else, let & assignment working ([5ab6d0a](https://github.com/atlas77-lang/Atlas77/commit/5ab6d0a24c16ce121c0ba65bb9f5df66179e990c))
- Codegen working for square.atlas ([8cf81ca](https://github.com/atlas77-lang/Atlas77/commit/8cf81ca25a004b88a9459edff90d65daca160d9a))
- The lowering pass is working ([dc4db1e](https://github.com/atlas77-lang/Atlas77/commit/dc4db1e4e6e1fb681628535b3fa005041963b913))
- Square.atlas runs ([2113acf](https://github.com/atlas77-lang/Atlas77/commit/2113acffc5318f188ae19d321da2e6dd76e6db11))
- Smol start of codegen + vm ([0198aba](https://github.com/atlas77-lang/Atlas77/commit/0198abaf8b1a709db1a56af4097f7227482751e2))
- Hello.atlas now work ([fc8d2f1](https://github.com/atlas77-lang/Atlas77/commit/fc8d2f18b856080db9d24b18407fc4f9a9ddbe77))
- Parser can now parse hello.atlas ([26e803a](https://github.com/atlas77-lang/Atlas77/commit/26e803a088abea86d9bf3078b8b52395f3006eb2))
- Let & binary op works ([647686b](https://github.com/atlas77-lang/Atlas77/commit/647686b1bf077193abe0125a1b836aaa11f1ed64))
- Upload built binaries to release pages ([a0a6c1a](https://github.com/atlas77-lang/Atlas77/commit/a0a6c1afa0f850b2ece63ced244fee510218e021))
- Implement new AST structure for program representation ([55713de](https://github.com/atlas77-lang/Atlas77/commit/55713de10e0a2a21185739570416c62b456461c2))

### Miscellaneous Tasks

- Cargo clippy ([9af5f25](https://github.com/atlas77-lang/Atlas77/commit/9af5f258eeafe2a38e0c0d5b9f4c9e35266b67dc))
- Doing what clippy wants ([0dbce18](https://github.com/atlas77-lang/Atlas77/commit/0dbce182cb37dec1d818aefb275a49070e9d4813))

### Refactor

- The language as a whole is done for the v0.5 ([ed16dd1](https://github.com/atlas77-lang/Atlas77/commit/ed16dd1fab49a314a21171e88863d5d2ca890643))
- The whole pipeline down to the VM works. ([508414e](https://github.com/atlas77-lang/Atlas77/commit/508414ec66d610c570e14b54483c63a5f94d9c7c))
- Everything is broken rn. I'm refactoring everything ([56adc11](https://github.com/atlas77-lang/Atlas77/commit/56adc11633cd5c75c192bce94262bdb811323f99))

### Misc

- Update ./examples ([fc5053d](https://github.com/atlas77-lang/Atlas77/commit/fc5053d37ff7a097aed4cf9426684711691620d4))
- Clarified the README.md ([390cca4](https://github.com/atlas77-lang/Atlas77/commit/390cca4e2ed8896a6f668497289f2a019defdd62))

# Changelog

All notable changes to this project will be documented in this file.

## [unreleased]

### Features

- Implement new AST structure for program representation ([55713de](https://github.com/atlas77-lang/Atlas77/commit/55713de10e0a2a21185739570416c62b456461c2))

