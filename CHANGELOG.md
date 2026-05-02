# Changelog

All notable changes to this project will be documented in this file.

## [0.8.1] - 2026-05-02

### Bug Fixes

- Float precision during codegen for integer float value ([6d8ade4](https://github.com/atlas77-lang/Atlas77/commit/6d8ade484a37f8bd5c2dcaf4417f6c44444d3ba2))
- Literal number types weren't properly lowered in the LIR ([16145b6](https://github.com/atlas77-lang/Atlas77/commit/16145b678b18befc7693f8b29ba45ab81ad2248b))
- Parameters are now actually check to know if the type exists ([c3d2b7a](https://github.com/atlas77-lang/Atlas77/commit/c3d2b7ae4710c03d67364434819804deffccb06f))
- Unions signature and declaration order were malformed in the codegen ([87e3789](https://github.com/atlas77-lang/Atlas77/commit/87e378929176de0e91dfd12f903303d575491590))
- Fields positions are now deterministic and stable across builds ([110d75e](https://github.com/atlas77-lang/Atlas77/commit/110d75e9b6c2fdbba8e7811013dfb46eccf117ec))
- Strings now properly represent special characters + unicode values ([3ea369d](https://github.com/atlas77-lang/Atlas77/commit/3ea369d76268a0cfc93bb847889bec65e38d2d13))
- Allow for `namespace::enum::variant` ([d204f99](https://github.com/atlas77-lang/Atlas77/commit/d204f99879695e5cd83cefebe1d05186794de155))
- Allow function pointer casting ([fc39cfe](https://github.com/atlas77-lang/Atlas77/commit/fc39cfe2067a750337902ade470303a33436a5d1))
- You can now get function pointers from namespaces' functions ([ed34e91](https://github.com/atlas77-lang/Atlas77/commit/ed34e9156c4112e8bc097c2dd4d56f1e1f236187))
- `atlas77 package` wouldn't add namespace names on object types where it was used ([43ae639](https://github.com/atlas77-lang/Atlas77/commit/43ae639d27a5858b10ddf83ef5356387aedeb61d))
- Different case warnings are now ignored for std types and functions ([87a9738](https://github.com/atlas77-lang/Atlas77/commit/87a97382c9b4f860b8b070c49de8f0aa34b05602))
- Trivially_copyable structs with a destructor are now rejected ([8134969](https://github.com/atlas77-lang/Atlas77/commit/81349694c218bc0e17c12e2a13b6b85486e6ff88))
- Return statement did not reset variable state in ownership pass ([307f681](https://github.com/atlas77-lang/Atlas77/commit/307f6815668538826b4f703d5317fdff088ff80b))
- Std c99 & clang warnings for the codegen ([a8fae44](https://github.com/atlas77-lang/Atlas77/commit/a8fae44fa5e70eefc8a9e23354f35c37ab86edf9))
- Method in generic structs are now lazily instantiated ([8e85794](https://github.com/atlas77-lang/Atlas77/commit/8e85794c9e76b253a8ac23558efcefdb35cf4d04))
- Variables weren't getting registered if their value type and declared type wasn't the same. Causing a lot of annoying cascading errors ([5fef9ea](https://github.com/atlas77-lang/Atlas77/commit/5fef9ea6ac136fabe2e77409b1c8c1544e7a6f18))
- Std::expected wasn't well formed ([915a010](https://github.com/atlas77-lang/Atlas77/commit/915a010ebe098b963c76b1ac075569551ff58ded))
- Most examples should now be working ([1b421e5](https://github.com/atlas77-lang/Atlas77/commit/1b421e51eb5565618aaa0b4a0d2709330ba27396))
- Removed all faulty "std::move" to replace them with "std::ptr::read". ([a67a048](https://github.com/atlas77-lang/Atlas77/commit/a67a048ee1afb64f64ef9aed19fb2ab9d87844d7))
- Allow empty object literal (e.g. `Foo {}`) ([577eba5](https://github.com/atlas77-lang/Atlas77/commit/577eba5f8d6e0da5bed58c71fbd9a38fff65e2a1))
- The ownership pass now also runs for methods and dtor ([c4906f9](https://github.com/atlas77-lang/Atlas77/commit/c4906f926561736105bfa1f42359051e7ddf92bf))
- Can only move from local variables now ([730029f](https://github.com/atlas77-lang/Atlas77/commit/730029f2418c287bd985aaed51ab14f3288c7513))
- Add back language for external items (e.g. `extern "C"`) ([e6ace4d](https://github.com/atlas77-lang/Atlas77/commit/e6ace4d3321eccf8253472f449b3446e2c572b65))
- Update the hello world example to use the std namespace ([67c89e1](https://github.com/atlas77-lang/Atlas77/commit/67c89e118fb7992be514badcf6984c682c9424dc))
- Recursive pointers to generics now error out ([7088b22](https://github.com/atlas77-lang/Atlas77/commit/7088b22b94c74ff44b1d15174b34db2f5656a4cb))
- Static access for classes in namespaces ([9307ddb](https://github.com/atlas77-lang/Atlas77/commit/9307ddb2a4a57695ee89405d13fbc416854b7711))
- Removed "name_should_be_in_different_case" warning for extern declaration ([346964a](https://github.com/atlas77-lang/Atlas77/commit/346964a8cdf41c96d5da2d3b289b031f35b4c6c7))
- Removed "`NoFieldInStruct`" error ([2cba551](https://github.com/atlas77-lang/Atlas77/commit/2cba5510e99098b61a856686a7e8f3bd5ba1c8d4))
- Issue with .dll-s not being included in output directory ([bf4ca9e](https://github.com/atlas77-lang/Atlas77/commit/bf4ca9e2a1f550a74860131bd104aa1c2bb2a808))
- Add `vendor/tinycc/include` path even on windows target for TCC ([9718402](https://github.com/atlas77-lang/Atlas77/commit/971840203160afe8ef62957beda05deba18eb1ed))
- Potential fix for GNU toolchain users ([34a4877](https://github.com/atlas77-lang/Atlas77/commit/34a48770fd41898ca9d445b45e2810ccacc50266))
- Skip block-exit RAII drops when block ends with return ([6e091a6](https://github.com/atlas77-lang/Atlas77/commit/6e091a6393f382dcece114e9e9031d137b3dd21c))

### Documentation

- Updated ROADMAP.md and README.md ([e9aaa3c](https://github.com/atlas77-lang/Atlas77/commit/e9aaa3c3ec616fc2e8fe01395f73f1be42621be1))
- Updated the roadmap ([af26a79](https://github.com/atlas77-lang/Atlas77/commit/af26a79110e8add91c0a459e967d81abfedfb5bd))
- Updated the README.md file ([c1810e7](https://github.com/atlas77-lang/Atlas77/commit/c1810e75304ea6390594ee9c69c8a4b577d2f78e))

### Features

- Ignore invalid variables to avoid cascades of errors ([33f2e1b](https://github.com/atlas77-lang/Atlas77/commit/33f2e1b0d22f8d39b59a99ec882eb30cd849e891))
- Add variadic type support in parser and lexer ([9767712](https://github.com/atlas77-lang/Atlas77/commit/9767712325923508062d9f3d7b0cd20b703ba73c))
- Implement intrinsic type_id ([13204cb](https://github.com/atlas77-lang/Atlas77/commit/13204cb8754c33e6086a9867ab649e660a7b743b))
- Add very basic reflections to the language ([47fc828](https://github.com/atlas77-lang/Atlas77/commit/47fc8282021a6ae62591b9c1ec73e34022678895))
- Add external unions to the laguage ([824ae73](https://github.com/atlas77-lang/Atlas77/commit/824ae73258fa87887ae014e4c072e7313237a7d6))
- Added back a very simple instant/duration API ([a71a217](https://github.com/atlas77-lang/Atlas77/commit/a71a217b1ee7c6ea1e27959b7ef0d701b29b3ece))
- Add string type instead of raw '*uint8' ([166760b](https://github.com/atlas77-lang/Atlas77/commit/166760b38afb14653b494e11286cd6950fe7e58c))
- Add support for special methods ([b85eba7](https://github.com/atlas77-lang/Atlas77/commit/b85eba764f9b9d8c2ad30c125b58dfce87210966))
- Add `std::hashable` constraint + warnings for badly formed special methods ([e2bef49](https://github.com/atlas77-lang/Atlas77/commit/e2bef49c92f7a368ff58a44799801e2c013b7de0))
- Added support for bitwise operators (`<<, >>, !, &, |, ^`) ([6708593](https://github.com/atlas77-lang/Atlas77/commit/6708593c2761414cc4141f2339380296723d8908))
- Added `std::alloc<T>() -> *T` as a safer wrapper over malloc(uint64) ([40facba](https://github.com/atlas77-lang/Atlas77/commit/40facbad6ff5f4649c53374e1c3f7ba667d98faf))
- Add `std::take<T>(*T) -> T` ([4f55120](https://github.com/atlas77-lang/Atlas77/commit/4f551206aee214b52a95953cce9841d76c5cf0bc))
- Add `std::map<K, V>` ([447a7f6](https://github.com/atlas77-lang/Atlas77/commit/447a7f69d41de1d5439687421971dd8aad0a38d0))
- Add nullable structs/methods modifiers ([9853b2c](https://github.com/atlas77-lang/Atlas77/commit/9853b2c3868b942bc4af087d8e7a22c98393e404))
- Add `std::either<L, R>` to the language ([b4cea81](https://github.com/atlas77-lang/Atlas77/commit/b4cea81b3599dd8fff73bc7f9adbe16d2958c544))
- Add method generics (FINALLY) to the language ([15c7a6a](https://github.com/atlas77-lang/Atlas77/commit/15c7a6a59324780cecd839ed8cece9879b73e39c))
- Add `std::ptr::read/write/swap` ([e34cbda](https://github.com/atlas77-lang/Atlas77/commit/e34cbda07bf22516d3c180bf57e85be4e467a682))
- Added String::push_str() + one string example ([bba261a](https://github.com/atlas77-lang/Atlas77/commit/bba261ad4104e146c04ee6f070426344debe671b))
- Add std::realloc/zero_alloc/calloc to std/memory ([bf7c321](https://github.com/atlas77-lang/Atlas77/commit/bf7c3216616044d76c43a534858d472b52f6991f))
- Added `#[c_name("name_in_c")` flag for external items ([3ac0d8f](https://github.com/atlas77-lang/Atlas77/commit/3ac0d8f53a0ee30ee36d8a0b913eee2d31ab81d0))
- Added std namespace for... std thingies ([2f7f17a](https://github.com/atlas77-lang/Atlas77/commit/2f7f17a7fb160da779defab738c85ff67d537b9d))
- Added automatic library generation from C header file into `atlas77-my_header.h`, `atlas77-my_header.c` & `my_header.atlas` ([320600a](https://github.com/atlas77-lang/Atlas77/commit/320600a734cfe13645cc7a370571487202f481d2))
- Add `source_dirs` for all sources directory like I did for lib_dirs or include_dirs ([e6aa6fb](https://github.com/atlas77-lang/Atlas77/commit/e6aa6fb9a2587b97a0d011ba46eb644d44f3a673))
- Implement very simple, stupid but working namespaces ([a3595b1](https://github.com/atlas77-lang/Atlas77/commit/a3595b1a56981513ca529f182090e52523af9c46))
- Add `atlas.toml` build configuration file ([6dbd347](https://github.com/atlas77-lang/Atlas77/commit/6dbd347eeb94c9628abfb9f1f057ed073024bbd6))
- Add generic function pointers `add<int64>` ([bf9f00e](https://github.com/atlas77-lang/Atlas77/commit/bf9f00e6014e383323ff7970ef00c75ae48a49b3))
- Implementation of function pointers ([98a2960](https://github.com/atlas77-lang/Atlas77/commit/98a29605dac39f600f2b676221a80b1dfe096427))

### Refactor

- Update LirOperand to include size information for immediate values ([ac860df](https://github.com/atlas77-lang/Atlas77/commit/ac860dffc4de79626aa28f50653ff236965272d6))
- Unify the attributes parsing a bit ([65540e3](https://github.com/atlas77-lang/Atlas77/commit/65540e39653a964960ccb2b665e775d89bbc9dfd))

### Misc

- Removed useless core/experimental/reflection file ([f477962](https://github.com/atlas77-lang/Atlas77/commit/f477962c0533cde01bb13e4830bec52798a5b642))
- Remove unused test.clif (should have been done earlier) ([2a39ee1](https://github.com/atlas77-lang/Atlas77/commit/2a39ee1b0820efc8d083a667b04c18c0ac4d0f6f))
- Removed NameShouldBeInDifferentCase warning ([ddddeb0](https://github.com/atlas77-lang/Atlas77/commit/ddddeb0577a8ae41e08aab7c5614b9b4a6e304d6))
- Removed all unused experimental libraries ([89962d4](https://github.com/atlas77-lang/Atlas77/commit/89962d4149d683d3154c07f3bf16b0aa9c718631))
- Removed raylib & blue-engine bindings ([a022c71](https://github.com/atlas77-lang/Atlas77/commit/a022c711b13abe9a3fc1714e10c6253e7098d90d))
- Small clean up of the compiler + enable of require drop union field warning ([7cb9019](https://github.com/atlas77-lang/Atlas77/commit/7cb9019364838f9965917e19d7018f7c1fbc8299))
- Precise if a stdlib is outdated or not ([f090124](https://github.com/atlas77-lang/Atlas77/commit/f09012442c3e08f358766b0381139ac084bb55e0))
- Trimmed down the atlas77 header and renamed it ([e240f7d](https://github.com/atlas77-lang/Atlas77/commit/e240f7dc4ea81c32d0cf03eafa07ae5e48325bd7))
- Remove raylib directory ([52960ff](https://github.com/atlas77-lang/Atlas77/commit/52960ff57022a500b266242311da785c8d860269))
- Made `normalize_library_name_for_runtime` only available on windows ([d955ca0](https://github.com/atlas77-lang/Atlas77/commit/d955ca068f4264deaae61b98b3a9f0e8ce1276b3))
- Move expected into the std namespace ([c672d1e](https://github.com/atlas77-lang/Atlas77/commit/c672d1e45987422fcc80de74127e986abd562eda))
- Move std/math into the std namespace ([25d5df3](https://github.com/atlas77-lang/Atlas77/commit/25d5df3aaafccb355cac298fbb4947eac98dd5d7))
- Trimmed down test.atlas ([377d173](https://github.com/atlas77-lang/Atlas77/commit/377d173865be76a8ba8bb590cf004afc92729332))
- Removed `self/` directory, this will be moved into its own repo one ([96ab999](https://github.com/atlas77-lang/Atlas77/commit/96ab99988f0d1d5102f9580f0ca9ff2b7465b74c))
- Cleaned up raylib library so every symbols are public ([c272ecd](https://github.com/atlas77-lang/Atlas77/commit/c272ecd1c116364a1e689e231be261542ae5325a))
- Bump all packages version to latest ([12e5bf0](https://github.com/atlas77-lang/Atlas77/commit/12e5bf07d3d64809590137b7ddbb73434b9e8cff))
- Remove `LirInstr::AggregateCopy` + fix `raylib/` import ([82ba69e](https://github.com/atlas77-lang/Atlas77/commit/82ba69e097cea9a2b953237754e98f080e22df4e))
- Clean up examples and stdlib ([1d50e0f](https://github.com/atlas77-lang/Atlas77/commit/1d50e0f21702d24ed5ab9add64f716cfdc70cb48))

# Changelog

All notable changes to this project will be documented in this file.

## [0.8.0] - 2026-04-02

### Bug Fixes

- Array of arrays not being codegen-ed at all ([f930a51](https://github.com/atlas77-lang/Atlas77/commit/f930a51e2c7d6de2c4106a99d637dad0880c88c1))
- Arrays of pointers are now properly codegen ([70b80bd](https://github.com/atlas77-lang/Atlas77/commit/70b80bd4d698a20f55e974d429098c3a25deac23))
- Delete statement of `std::trivially_copyable` types are now no op ([a61772d](https://github.com/atlas77-lang/Atlas77/commit/a61772dc414e8c87965c87fef352244c96f7c03b))
- Empty return statement became `return NULL` ([e621bc0](https://github.com/atlas77-lang/Atlas77/commit/e621bc019862ff6b8c75a616d01725553d47e92b))
- LIR lowering would complain when the  block wasn't returning something ([b0a4412](https://github.com/atlas77-lang/Atlas77/commit/b0a44121bd1dacc63c2c6430dd365958b8c16af9))
- Removed `ptr` as a keyword in the parser/lexer ([a55ec89](https://github.com/atlas77-lang/Atlas77/commit/a55ec890a62efe518437d7ceeb8c55a904e0e4fb))
- Std math is now C compliant ([1899743](https://github.com/atlas77-lang/Atlas77/commit/18997438b6c8b599edecd529851c81b50baad371))
- Std expected is now v0.8.0 compliant ([f982f6f](https://github.com/atlas77-lang/Atlas77/commit/f982f6f2e4e5801a5da6a2840c308299699f73b6))
- Small regression with intrinsic move function ([91ab03b](https://github.com/atlas77-lang/Atlas77/commit/91ab03bdc550d7dc537b06c896d4cbd433440b52))
- Bug fixes + const pointer errors/warnings ([052fec3](https://github.com/atlas77-lang/Atlas77/commit/052fec383cfbf950b761b3c4b140b2e564df16f7))
- Allow optional rvalue transfer and order C type definitions ([9df4740](https://github.com/atlas77-lang/Atlas77/commit/9df47408b9c159c1cfe7c457bf1fecb5ea51dab3))
- Temporaries and variable reassignment now gets properly deleted ([80fa521](https://github.com/atlas77-lang/Atlas77/commit/80fa521910b916b44ff2a67dbbbc503796dc7bec))
- The default constructor didn't actually exist ([4103d55](https://github.com/atlas77-lang/Atlas77/commit/4103d55bcc35441f28eb628f9f4121911c9adb0f))
- Pointer constness lowering and generic instantiation discovery; tighten type-check semantics ([289429e](https://github.com/atlas77-lang/Atlas77/commit/289429e8c2039f0955a3416eb7453c5f2e437247))
- Static method would take an implicit "this" ([525ea38](https://github.com/atlas77-lang/Atlas77/commit/525ea38cbc53d57db6a9eebfa0bfe08ff1cd6c78))
- Tests were broken ([e592c6c](https://github.com/atlas77-lang/Atlas77/commit/e592c6c5a59e423eb226953043e39ff74decc9b4))
- Constructors had issues with naming scheme ([2e405b9](https://github.com/atlas77-lang/Atlas77/commit/2e405b9830fe2eab61bf6a5c772972490a273596))
- Removed all references ([127f9b9](https://github.com/atlas77-lang/Atlas77/commit/127f9b9f233dddd800dcc7ae56dfea6537b29b3b))
- `__atlas77_c_from_chars` in useful_header.h ([a737f6e](https://github.com/atlas77-lang/Atlas77/commit/a737f6e2f12a125e3f3dc00f15797faee8b6152b))
- Update the signature of the extern functions ([7c3b4d1](https://github.com/atlas77-lang/Atlas77/commit/7c3b4d1110bda2797f102989a23e638b4e57fa79))
- Attempt to make smart_ptr works again ([149d304](https://github.com/atlas77-lang/Atlas77/commit/149d304ac7516aad58eeb28bad2a7c015a055824))
- Extern fn in type checker now correctly handles generic references ([ed8d29e](https://github.com/atlas77-lang/Atlas77/commit/ed8d29ef43dd5a84f0ccb4d44d5669790e9ddf2d))
- Now the compiler crashes beautifully when an intrinsic is not known + You can now use the type  in generics. It crashed before because of the  function in the parser ([abc2cf4](https://github.com/atlas77-lang/Atlas77/commit/abc2cf44199a74bba6d0286c340a88fb9ff1d37b))
- Fix of array & slice. They should work once all their features are actually implemented. ([33f9e31](https://github.com/atlas77-lang/Atlas77/commit/33f9e319f5483af7c13d9d7a7f7ecac18249b808))
- Removed `&this` "optimization" in the typechecker for the old VM ([4aa39f4](https://github.com/atlas77-lang/Atlas77/commit/4aa39f49d7c164b17b0a7a4d0a74386d3a92882c))
- It actually already existed, I am stupid ([04530c4](https://github.com/atlas77-lang/Atlas77/commit/04530c477aefb73fde0f15458045fe2cd4f4c289))
- Issue with short & long helps in the CLI for `build` ([88af79d](https://github.com/atlas77-lang/Atlas77/commit/88af79df8a6a1ff576a61faac5268454ef81fab4))
- Added back header include for linux-x64 platform ([1e4e03d](https://github.com/atlas77-lang/Atlas77/commit/1e4e03dc91b3ba2cb556dadf917b5c2fe38ba22a))
- Better CLI experience ([c8036b1](https://github.com/atlas77-lang/Atlas77/commit/c8036b1a6fcfffb3af656f3455fe723f8a511e33))
- Windows prebuilt binaries finally work ([306d96b](https://github.com/atlas77-lang/Atlas77/commit/306d96b81b3b845de73ef3ecda20b7db4b5e456b))
- I can finally compile on linux-x64 properly. ([f4102de](https://github.com/atlas77-lang/Atlas77/commit/f4102de569b358f136aeb1c4302b2f93bc98a1dc))
- Added linux-x64 prebuilt ([7b06bea](https://github.com/atlas77-lang/Atlas77/commit/7b06beab7fe1834e13c70b7b5fc9a8a17e5e5e03))
- Build prebuilt binaries ([b3c47ea](https://github.com/atlas77-lang/Atlas77/commit/b3c47ea6bcb7a431a84aa3635626aadd1f6664bf))
- Build-tinycc.yml ([b335614](https://github.com/atlas77-lang/Atlas77/commit/b3356145a9a4131e7773b2ee788326a3b3a6cb2e))
- More tries on build.rs ([bac0ea9](https://github.com/atlas77-lang/Atlas77/commit/bac0ea958ffdb3c31e47239de00f3e6d666b9d5e))
- Trying to fix build.rs ([0079cda](https://github.com/atlas77-lang/Atlas77/commit/0079cda1f492afb237d490e42d9200cf62bc8f79))
- Call expr return types once again ([97e00de](https://github.com/atlas77-lang/Atlas77/commit/97e00de4c5e1f06bcff1a840f309cfcd229b4b46))
- Extern call expressions are now typed (yes they weren't for some reason) ([910b870](https://github.com/atlas77-lang/Atlas77/commit/910b87074d77b760711247ab537a9a2199012659))
- Returned identifier now doesn't need to be copied (weird issue, I am just too dumb, don't worry) ([e757fc4](https://github.com/atlas77-lang/Atlas77/commit/e757fc4e035e0c25548f64ae8f67b1a62f95273b))
- Mangled names now use only alphanumeric + '_' characters ([3329506](https://github.com/atlas77-lang/Atlas77/commit/3329506475da9c5c2088612b2ab8f6dd5441f184))
- Functions should be properly stored in the module now ("should") ([d7bac67](https://github.com/atlas77-lang/Atlas77/commit/d7bac6791a2c5de7ae1243536fd2e10d9bbf4a6a))
- The parser can now properly parse deref assign ([ac42a9a](https://github.com/atlas77-lang/Atlas77/commit/ac42a9a7ab2120abf28f72a1007e0c41ca76b69f))

### Documentation

- New Roadmap ([91a6753](https://github.com/atlas77-lang/Atlas77/commit/91a675349af82ee08595eb6b6e59775a177b3f31))
- Created ./specs/ with Document 0 & README ([781d1b4](https://github.com/atlas77-lang/Atlas77/commit/781d1b4b8224def700def838a5409535b93ee3e7))
- Wrote a lot in SPECIFICATIONS.md ([b2fb1df](https://github.com/atlas77-lang/Atlas77/commit/b2fb1dfd103f581916b4874fdb7704fdbf5c1629))

### Features

- Somewhat stabilize the shared_ptr implementation ([27dd7e8](https://github.com/atlas77-lang/Atlas77/commit/27dd7e8e57e2223c8a04be3c560e6f86cb9f77bc))
- Std Vector<T> & optional<T> are now working again ([ead261e](https://github.com/atlas77-lang/Atlas77/commit/ead261e5483fb1252f8e8d1a11e69cccb73e96cf))
- Expand the raylib feature and made the Camera2D example works ([877c215](https://github.com/atlas77-lang/Atlas77/commit/877c215273ce81c4bfe6c29622f470abaa726bfa))
- Add support for array literals and `std::trivially_copyable` struct flag ([2c82623](https://github.com/atlas77-lang/Atlas77/commit/2c82623d8860fccc5408777e43058d5421a04163))
- Raylib is working (at least on Linux) ([c61e36e](https://github.com/atlas77-lang/Atlas77/commit/c61e36efb1c79d79e0f06212fd9c44a288b35f2f))
- Improve compiler diagnostic and introduce intrinsic move(from) ([2269517](https://github.com/atlas77-lang/Atlas77/commit/2269517413650329864222f674758b7067961389))
- Improving the stdlib ([cfb693f](https://github.com/atlas77-lang/Atlas77/commit/cfb693f6b8b2a9c769ce98fe24916d1c798d29b8))
- Add std::optional, an experimental reflection library and expand on std::io ([c2ab44b](https://github.com/atlas77-lang/Atlas77/commit/c2ab44b85af1f1c75bb39b57c6e2a1c0e7bd5e17))
- Added input() and all its used features ([f145d60](https://github.com/atlas77-lang/Atlas77/commit/f145d603239450d514b17f619421b12c02137ea8))
- Migrate std string and time to pointer-based u8 runtime APIs; add memory module and namespaced C hooks ([49a5f24](https://github.com/atlas77-lang/Atlas77/commit/49a5f24e85ff1d203d8f2a62bb951dda0afeea9a))
- Add logical ops, fix receiver lowering and align C emission with u8 string model ([74e2e50](https://github.com/atlas77-lang/Atlas77/commit/74e2e5021fabab2b5a84a935927a572e0e3d575e))
- Improve Token spans, escaped char literals, const-type handling and trailing comment recovery ([1600bbe](https://github.com/atlas77-lang/Atlas77/commit/1600bbef66800e56a8c08da3c557a09864d1a924))
- Added the groundwork for raylib ([0193801](https://github.com/atlas77-lang/Atlas77/commit/01938018b7f93963c9a7ff4235aab3411b33cdd0))
- Added Literal type promotion (e.g. int16->uint8) ([9223f89](https://github.com/atlas77-lang/Atlas77/commit/9223f899e636f86a6d0817f305ad924ff7c9d530))
- Support for external structs ([0ef3675](https://github.com/atlas77-lang/Atlas77/commit/0ef36755174488a439fd0b4b638a1a3880ac988f))
- Added stack allocation for structs ([0e4da05](https://github.com/atlas77-lang/Atlas77/commit/0e4da05f67862009b1598617af6db89c23f0a35b))
- `size_of` operator actually works now ([528741e](https://github.com/atlas77-lang/Atlas77/commit/528741e69b4c193b0f645692bb72ddf4ef279c69))
- Add a simpler 'ownership' pass ([27905a4](https://github.com/atlas77-lang/Atlas77/commit/27905a4872250a629db2172d969f3a0f5ce94442))
- Full new reference model implementation. ([c119bcf](https://github.com/atlas77-lang/Atlas77/commit/c119bcf7882e3903390f61647eaffe4dd1a7d1b0))
- New (very unstable) owner ship pass and removed the old reference model ([2b25ad3](https://github.com/atlas77-lang/Atlas77/commit/2b25ad30ec496cb42cd9e49d2696164dd648caa9))
- Continue working on the references. ([18064e1](https://github.com/atlas77-lang/Atlas77/commit/18064e1e9d3dd5baf1cc527fc8d354bf3652e65d))
- The new ref model is supported everywhere now, but isn't properly working ([54bc76e](https://github.com/atlas77-lang/Atlas77/commit/54bc76ec8e181163615d16318e7f1cb364ac552b))
- Can lower the new reference model ([5da357d](https://github.com/atlas77-lang/Atlas77/commit/5da357d665de87eab77fe74b7ec5d55f7c0c17f5))
- Can parse the new reference model ([1a5c2d6](https://github.com/atlas77-lang/Atlas77/commit/1a5c2d6099f7abd93eabeb2036e0996fb7ea0121))
- Added `size_of` & `align_of` operators ([123f6fa](https://github.com/atlas77-lang/Atlas77/commit/123f6faa210fa68d0971748fa82a10e6511a6e69))
- Inline arrays are now working in C ([5f46055](https://github.com/atlas77-lang/Atlas77/commit/5f46055bc115cf7d31fb0553c4d4b4931a415aeb))
- The type `[T; N]` is not proper inline storage. ([00c745c](https://github.com/atlas77-lang/Atlas77/commit/00c745ce27948d5b779368bac64d09e83eb757ef))
- Reworked std/experimental/array & worked more on std/experimental/slice. Very goofy, but it's very interesting imho ([608a38b](https://github.com/atlas77-lang/Atlas77/commit/608a38bc911e9e2d21287ae74bd05907b25318e1))
- Added std/experimental/slice.atlas, very funny abomination lmao ([4917e3e](https://github.com/atlas77-lang/Atlas77/commit/4917e3e3bdac007c35b6b2e858272f84688d91de))
- Added 8/16/32 bits variant of int/uint/float ([85f0eff](https://github.com/atlas77-lang/Atlas77/commit/85f0eff83f2abc2c502b0b51ba42a334e5194906))
- Added slice and fixed length arrays ([06baebe](https://github.com/atlas77-lang/Atlas77/commit/06baebe653604355682905d40a11d0e8639f6899))
- Hashmap robin hood implementation ([8f18ae4](https://github.com/atlas77-lang/Atlas77/commit/8f18ae40a906ad9bdcd0e948a4a80a362bb6a7d9))
- Unions are now supported ([1f4682f](https://github.com/atlas77-lang/Atlas77/commit/1f4682f0e7702390aa3e1356dbc1774400c49f29))
- Structs are now being properly codegend. std/experimental/c_vec.atlas is working. ([e7d6e6c](https://github.com/atlas77-lang/Atlas77/commit/e7d6e6c4caa3281e846b56b0214bc645cedc6a15))
- Basic support for structs ([e6477ac](https://github.com/atlas77-lang/Atlas77/commit/e6477ac553187b4bb8b7d29937ed600b8ecab425))
- Added a warning if a user tries to use TCC when it's unavailable ([29a702b](https://github.com/atlas77-lang/Atlas77/commit/29a702bf04f389c773597ad367e49440309b7fe2))
- If TCC isn't embedded, it will try to call the one installed on the user's system. ([108839e](https://github.com/atlas77-lang/Atlas77/commit/108839e127e2f6cd4d1621e9765d35ac0312a0d9))
- Added the Intel C compiler ([24f5800](https://github.com/atlas77-lang/Atlas77/commit/24f580061745adc08df3b0a8408e09981c0dc781))
- Support for multiple C compiler ([ac010fe](https://github.com/atlas77-lang/Atlas77/commit/ac010fea7741fff1b60f800bb5afcec0958a4d77))
- Support for both the MSVC & GNU toolchain ([36a71aa](https://github.com/atlas77-lang/Atlas77/commit/36a71aab51a7a4fff587001f09a54f66aeddf842))
- Windows-x64 prebuilt done ([6a7223b](https://github.com/atlas77-lang/Atlas77/commit/6a7223b4bd6b4c3dc7229d5b1873580ff5767eb2))
- Added tinycc ([40c526f](https://github.com/atlas77-lang/Atlas77/commit/40c526f20d4ae5b9a0b71ea20098bc5f892f8716))
- Vendor TinyCC as submodule ([e885844](https://github.com/atlas77-lang/Atlas77/commit/e885844368d915942cc0cd54eaaea652db912ee8))
- Swap function works now ([d7dfaea](https://github.com/atlas77-lang/Atlas77/commit/d7dfaeafa4bdf0864fc8ea969018d02ef5f6f089))
- Warnings for "unmanaged" types have been added in the Hir Lowering Pass ([8dd325c](https://github.com/atlas77-lang/Atlas77/commit/8dd325c4e0ea8e9edfefd9285c54efa375ca68fe))
- Changed the ownership system to be more inline with C++ semantics. Still a lot of works to be done ([e2687d1](https://github.com/atlas77-lang/Atlas77/commit/e2687d12f8bcc6aae5e177e14a4358784274ed16))
- References are now supported ([c62af15](https://github.com/atlas77-lang/Atlas77/commit/c62af157d388a2269ad89ac265a4f09c26b824f9))
- Now support about everything a function can do. ([bedc059](https://github.com/atlas77-lang/Atlas77/commit/bedc059b10efc71bce0d6e8d5ba228ab7eeba758))
- Hello World now works, and more instructions have been added ([f971f97](https://github.com/atlas77-lang/Atlas77/commit/f971f97890ac69558f7fa5a207b5ce1d2dc310a8))
- Very simple C codegen in place (works for fib only for now) ([f136ced](https://github.com/atlas77-lang/Atlas77/commit/f136cede2a566911be786119bc51fe79f50e496c))
- Added .gitattributes to get simple syntax highlighting on Github. ([4a11d25](https://github.com/atlas77-lang/Atlas77/commit/4a11d25785072a6be3d6c8621b230fea9e7f2a18))
- Added cranelift and a very very minimal codegen ([9e28e78](https://github.com/atlas77-lang/Atlas77/commit/9e28e7820c785c1c542f35929d35974d6f7632b1))
- Added support for __dtor, __ctor, __mov_ctor, default, copy in the typechecker ([8380c26](https://github.com/atlas77-lang/Atlas77/commit/8380c263189cabe555c3a1ec487b9bfe668b56e2))
- Support move/default constructor in the parser ([f418da3](https://github.com/atlas77-lang/Atlas77/commit/f418da38b4996163da64507aa40c63c2e2d66a42))

### Miscellaneous Tasks

- Bumped tinycc vendor to latest commit ([8f6bf2a](https://github.com/atlas77-lang/Atlas77/commit/8f6bf2a95174acc8b2538a7efe5cbbbce40844ac))
- Removed the old reference model ([65a4391](https://github.com/atlas77-lang/Atlas77/commit/65a4391813fa961251697d803245e43b74584abb))
- Cargo clippied the compiler ([9d4926c](https://github.com/atlas77-lang/Atlas77/commit/9d4926ceebc2c0dee6fcf6522cf69c9ce73cc11c))
- Removed unnecessary build scripts ([bb25020](https://github.com/atlas77-lang/Atlas77/commit/bb250208ea59722fd142bcbff29f17097931e584))
- Ran cargo clippy ([6bf7534](https://github.com/atlas77-lang/Atlas77/commit/6bf7534c7b6afc6ef782ed662b78fdcff19fe35f))

### Refactor

- Added a new ownership pass with RAII and value type semantics ([2a8b25b](https://github.com/atlas77-lang/Atlas77/commit/2a8b25b6807ea02a7d2f08ccdda862934895a139))
- Trimmed down the language by a lot ([9e1b66f](https://github.com/atlas77-lang/Atlas77/commit/9e1b66fbdfe7e90ca3e14af95987f4b7634dc83a))
- Removed the VM and all its codegen ([0447733](https://github.com/atlas77-lang/Atlas77/commit/044773313ee85cdd80b32d3d568f4c3b23c6f1bc))
- Even more changes to the ownership pass. It should be working now ("*should*") ([37e523b](https://github.com/atlas77-lang/Atlas77/commit/37e523b03975fe06957f8427634135b3f4078824))
- Removed cranelift entirely ([78cc06a](https://github.com/atlas77-lang/Atlas77/commit/78cc06a8f2b9fe01b0e22a441455c528ca00e7a9))

### Misc

- Removed the main function from every std lib files ([fbf0cf5](https://github.com/atlas77-lang/Atlas77/commit/fbf0cf5a0d06997e17b51be065e5cc4d9f90242e))
- Cargo clippy-ed the code ([386192c](https://github.com/atlas77-lang/Atlas77/commit/386192c90d60b917295fa3c5cc8c42b55391ee28))
- Continue to improve raylib support ([5873657](https://github.com/atlas77-lang/Atlas77/commit/58736576f8c4a16bc3cb860ba4e7d37894176adf))
- Add pass-aware dumps and monomorphization snapshot ([2ddd125](https://github.com/atlas77-lang/Atlas77/commit/2ddd12597bd8f2a91ef8315e24e77d2cdb92611b))
- Remove `pub (crate)` everywhere ([e022b5e](https://github.com/atlas77-lang/Atlas77/commit/e022b5ef7f05c13f3df6e9b08e81395abe342b10))
- Stuff ([031ce6d](https://github.com/atlas77-lang/Atlas77/commit/031ce6db6a8ad76af7496dc32ddc81f833a2ea85))
- Added notes for the changes of the new expr ([6193406](https://github.com/atlas77-lang/Atlas77/commit/61934066704fe33612444514a26b6ddfefeb4ad7))
- Added some thoughts in examples/hello.atlas ([92ed89c](https://github.com/atlas77-lang/Atlas77/commit/92ed89cff495134d009176ee2968a4818bf089d3))
- Removed build-tinycc.yml (we build them manually now) ([2cd29f6](https://github.com/atlas77-lang/Atlas77/commit/2cd29f65ffb310c4d1c273abd99cd0d272d0cc10))
- Added more test (don't mind them, they are constantly changing) ([b0e389c](https://github.com/atlas77-lang/Atlas77/commit/b0e389ce7ab3afc7e520347960caefa965ad8493))
- Added base.c ([ac47b70](https://github.com/atlas77-lang/Atlas77/commit/ac47b70ede5ad3bd8fa097d93fda4cf609c1c0b5))
- Trying to do stuff with cranelift, but it doesn't really work... ([6a51b08](https://github.com/atlas77-lang/Atlas77/commit/6a51b089c576a9f7b46365c9570e6c1ae253d083))
- Cargo fmt (I tried doing things in the new codegen, but it didn't work) ([6afc781](https://github.com/atlas77-lang/Atlas77/commit/6afc7813b5cbb374caa919daad2778bfb013f5b5))

## [0.7.3] - 2026-01-18

### Bug Fixes

- `std/time` still used the old copy constructor ([5fee875](https://github.com/atlas77-lang/Atlas77/commit/5fee87563e26c5bee8d5d07a1afdae4db136bb18))
- Removed the first whitespace if it exists ([86c48c3](https://github.com/atlas77-lang/Atlas77/commit/86c48c336c3b33d926ef6f87dd828bfc94baac66))
- README.md typo ([c13bfe0](https://github.com/atlas77-lang/Atlas77/commit/c13bfe02e4b8862369ab02c7547227a7c8a3ebf9))
- Well, it's tested now, and crashes at the very end after working, will investigate later ([48aebdc](https://github.com/atlas77-lang/Atlas77/commit/48aebdce4a1dfef6d77f0faef07790eeae27aebd))
- Track values deleted in loops ([eaa0d05](https://github.com/atlas77-lang/Atlas77/commit/eaa0d058ec73f274a8c997dd71be9fed03b2b9f2))
- Potential fix for #149 ([0953339](https://github.com/atlas77-lang/Atlas77/commit/095333952d1e0576412e9766b4fe6d1cebdddf2a))
- Well, another issue here, but that's just cuz I am stupid ([3aba518](https://github.com/atlas77-lang/Atlas77/commit/3aba5181481c1cbd1c5083d733eef1c06504ced8))
- Every values should be properly deleted before their assignment. ([b55f3c6](https://github.com/atlas77-lang/Atlas77/commit/b55f3c646cbbbb0c49fac80118401ab76a55f3c6))
- The typechecker didn't create a new scope for blocks ([1c4696f](https://github.com/atlas77-lang/Atlas77/commit/1c4696f9198cc86d7287f7d1f8cd7a5b1e8f7c83))
- The previous value of a variable wouldn't be freed if you assign a new value to it ([73711af](https://github.com/atlas77-lang/Atlas77/commit/73711af2f0a1d49d60d75a2f176f3e6679988edc))
- Temporary fix for #153, the compiler just rejects references that have a bigger depth than 2 ([11de88e](https://github.com/atlas77-lang/Atlas77/commit/11de88e49077323bdc7700386ec67572cef05d50))
- Std issue template ([b9927a3](https://github.com/atlas77-lang/Atlas77/commit/b9927a381ed3097282b72504f2f8c2d2a1070b57))
- Variables are now properly being lowered to the Lir without having to create a temporary ([e5af4b5](https://github.com/atlas77-lang/Atlas77/commit/e5af4b56b69e872a2b97f1964eadc17c807e58e8))
- The monomorphization pass checked the satisfaction of generic constraints before the copy constructors were generated ([3faf9e9](https://github.com/atlas77-lang/Atlas77/commit/3faf9e93409d6368f0e631d2f9225e414e14cf22))
- The monomorphization pass still used the old `_copy` method for the copy copyability checks ([b11c38d](https://github.com/atlas77-lang/Atlas77/commit/b11c38d7d8f8b747cdbb0f20b36b59a6883b03c0))

### Documentation

- Updated ROADMAP.md before new version ([534fc62](https://github.com/atlas77-lang/Atlas77/commit/534fc620b98698d49119dc97245a0d0d44d1a964))
- Added documentation to most files, still not done. Hoping to use this to generate the v0.7.3 documentation and the future ones too ([c04696d](https://github.com/atlas77-lang/Atlas77/commit/c04696dd235c66e5b28325eca1aa53f8cad25c43))
- Added a little documentation for people who would discover atlas from docs.rs ([a5e4fa3](https://github.com/atlas77-lang/Atlas77/commit/a5e4fa34fe8226d69e5d2b3e9a2ed307b4f19ca7))
- Updated the roadmap ([9071345](https://github.com/atlas77-lang/Atlas77/commit/90713451aa3ef8d529ea7d6f6b9985f0da6b2a2a))
- I believe, now there should issue/PR templates, & contributing template ([7b7f81e](https://github.com/atlas77-lang/Atlas77/commit/7b7f81e39dffa829031e950e7908bc848e115651))
- WELL I FORGOT TO UPDATE THE CONTRIBUTING_GUIDELINES. ([454cbda](https://github.com/atlas77-lang/Atlas77/commit/454cbdabae4848849706c32a67730df4675abdcb))

### Features

- Added multi line comments ([322415e](https://github.com/atlas77-lang/Atlas77/commit/322415e02feec71973dde2470725f83dcc72c079))
- Better documentation system. Will continue to improve it ([7035c6d](https://github.com/atlas77-lang/Atlas77/commit/7035c6deb233c79c0c73006be602dfac97663aff))
- Better documentation generation ([d91c436](https://github.com/atlas77-lang/Atlas77/commit/d91c43625e36d98c908b25ed08e8e88c9916baaa))
- First try at `atlas_77 docs`, still very wanky ([cf39c6e](https://github.com/atlas77-lang/Atlas77/commit/cf39c6eb91c00be97f029de5667098658c4a3525))
- Supporrt for `[` & `]` operators in brainfuck (not tested) ([f9edc5a](https://github.com/atlas77-lang/Atlas77/commit/f9edc5a6d699b2c1be20eec8577d993498c2b17f))
- Added reverse methods for `Iter<T>` ([592bfe1](https://github.com/atlas77-lang/Atlas77/commit/592bfe13fc41edd07b7c6ff07f9c128f7dee6392))
- Improved automatic destructor generation by rejecting union fields ([faba3bf](https://github.com/atlas77-lang/Atlas77/commit/faba3bf4021092ecaef1b0f83ed2589d7ae38618))
- The type checker reject variable shadowing. ([f98f1f2](https://github.com/atlas77-lang/Atlas77/commit/f98f1f26a17aa5352b397a7ab1e27882da2c5d8f))
- Added `std/experimental/iter` for a better iterator ([288e8fb](https://github.com/atlas77-lang/Atlas77/commit/288e8fb2e7d9650c27388019ede1160e62d94833))
- Added `ldimm %imm()` instruction ([6d5c5de](https://github.com/atlas77-lang/Atlas77/commit/6d5c5ded030d9cd8424076ff70d59336365f789b))
- The std has been updated to use the new where clause for the copy constructor ([416adae](https://github.com/atlas77-lang/Atlas77/commit/416adaeb0df0ebfa7734df8c8d8b1f6bb9b29666))
- Added checks for constraint satisfaction in the ownership pass (future proofing it) ([2b15c44](https://github.com/atlas77-lang/Atlas77/commit/2b15c44d2441955fa6605af63d0153a535d3ce44))
- You can now add where clauses on method and copy constructor for generic structs to have a conditional implementation of that method/copy_ctor on the struct ([22713f8](https://github.com/atlas77-lang/Atlas77/commit/22713f859346bd532c758dc90ca88bd568a2d98a))
- Added `std/experimental/future` for, well, the future ([ca46671](https://github.com/atlas77-lang/Atlas77/commit/ca46671d1b63cd4c3c03437efd161ea9545dfc25))

### Miscellaneous Tasks

- Cargo clippy & fmt ([dff7824](https://github.com/atlas77-lang/Atlas77/commit/dff782403640584d1cfecc0df4df88487d45816a))
- Cargo clippy ([27f8a63](https://github.com/atlas77-lang/Atlas77/commit/27f8a636db9d5ceac4af1dd78be5e1030fd56a73))
- Bump to `v0.7.3` ([b625376](https://github.com/atlas77-lang/Atlas77/commit/b6253766bdc4c710d252a12a14033ad3293e9b49))

### Refactor

- Assignments are now statements instead of expressions. Also fixed an issue where you could delete owned & non copy variables in loops. ([9d12503](https://github.com/atlas77-lang/Atlas77/commit/9d12503eee4f8f32d23c0d7cbe9eebe92cb6636b))

### Misc

- Changed the stack from `[VMData; Size]` to `&'run mut [VMDATA; Size]` ([245b1ea](https://github.com/atlas77-lang/Atlas77/commit/245b1ea3143429da89a8eae0d71c30ea06626d86))
- Some changes to std/experimental/future ([accfddc](https://github.com/atlas77-lang/Atlas77/commit/accfddc483f6c2e6a2d823ef474cb7594477728a))
- Improved error readability ([18e6bed](https://github.com/atlas77-lang/Atlas77/commit/18e6bed4acade49bdd0f5798c2390798fb7a2cd7))
- Added `optional<T>.as_ref()` but only in comment because of #153 ([d1648a5](https://github.com/atlas77-lang/Atlas77/commit/d1648a517535076ad4cee4e3b65cf074b543bbbb))
- Pretty printer now output the where clauses ([7acfeb7](https://github.com/atlas77-lang/Atlas77/commit/7acfeb72c98aabc00be2f0b73c80ec765d6cffe3))

# Changelog

All notable changes to this project will be documented in this file.

## [0.7.2] - 2026-01-11

### Bug Fixes

- Prevent double-delete by tracking deleted variables ([f0bcd9f](https://github.com/atlas77-lang/Atlas77/commit/f0bcd9f08648d9f76bf4bae1c589e3a30ee0814f))
- Mutable ref can't access consuming method ([cb2f565](https://github.com/atlas77-lang/Atlas77/commit/cb2f5655df08d016a96ddea406dd31258a9a463f))
- Preserving mutable references for types ([18f4e80](https://github.com/atlas77-lang/Atlas77/commit/18f4e80c1a3d19cb255b930ceed9b2b3da12987e))
- Union variants now preserve references when getting accessed by field ([cd06e2a](https://github.com/atlas77-lang/Atlas77/commit/cd06e2a57d4a1ad2377b2f9a91d408ee447168e7))
- Trying to access a copy constructor when the constructor was private would result in a bad error message ([322add4](https://github.com/atlas77-lang/Atlas77/commit/322add48bc2e98e6a516bb153f689e5c8f59ea91))
- Cannot move out of `std::non_copyable` references ([8d4615d](https://github.com/atlas77-lang/Atlas77/commit/8d4615d43b669c1f2bbe23be626e7e64df388adc))
- Codegen now support direct union variant assignment ([23995c5](https://github.com/atlas77-lang/Atlas77/commit/23995c5548d6b6a54cfb4a135292b5505dfaf959))
- If/else & while temporaries in condition weren't extracted ([5664f83](https://github.com/atlas77-lang/Atlas77/commit/5664f838cfbfa7a402185dba0bff59b54c4b5851))
- Issue with order of deletion of temporary variables ([4a67d7b](https://github.com/atlas77-lang/Atlas77/commit/4a67d7ba8e40ea542fd8f272524e1b2933dc8644))
- References can't escape to constructors/functions if their origins have been deleted/moved ([0c3c256](https://github.com/atlas77-lang/Atlas77/commit/0c3c256a69ec4f198fcccbdb3c099c39d2bec41f))
- The compiler would try to mangle function name even for external functions ([a6f2158](https://github.com/atlas77-lang/Atlas77/commit/a6f215878a303bf8878693c0b579328942cbc0ac))
- Copy constructor wasn't generated if there was an enum field in a struct ([bb85e39](https://github.com/atlas77-lang/Atlas77/commit/bb85e39658e52f9f72b36c8820d762767825ec23))
- `Iter<T>.next()` wouldn't move out `T` from the underlying Vector, which causes use after free in `T` destructor ([4fb71af](https://github.com/atlas77-lang/Atlas77/commit/4fb71afdeebba3f5ce9c5b8c82fc94c9d679ca46))
- Compiler would try to generate delete statements for enums ([571d3f7](https://github.com/atlas77-lang/Atlas77/commit/571d3f73df4856290b1d6e4307aa4b508e3b9fd2))
- Temporary values that need to be deleted in casts and method chains are now properly unwrapped into temporary variables ([c7557d2](https://github.com/atlas77-lang/Atlas77/commit/c7557d27eae2e170489f53a453ed470526b33f42))
- No flag and a destructor will result in no copy constructor ([57a7e29](https://github.com/atlas77-lang/Atlas77/commit/57a7e294f8bc2461f706351fe5a0289998eefb07))

### Documentation

- Updated the ROADMAP.md ([78ef9d3](https://github.com/atlas77-lang/Atlas77/commit/78ef9d3509138d2209fe9c0c7353fe6b8af2e68b))

### Features

- BIG SURPRISE ([973e737](https://github.com/atlas77-lang/Atlas77/commit/973e737b487161f9048fb79fce548ac02841030a))
- Added a copy constructor to `optional<T>` ([09b8dd1](https://github.com/atlas77-lang/Atlas77/commit/09b8dd1dbe3bfed08dc3d01eeeb91b010c6b8857))
- Added blocks to the language ([4f9ee16](https://github.com/atlas77-lang/Atlas77/commit/4f9ee16f7507cae191324c2731bba1bec0d6175f))
- The copy constructor is now callable `new MyStruct(&my_struct)` ([4db580b](https://github.com/atlas77-lang/Atlas77/commit/4db580b438de072821d96088b75e12c742fd868c))

### Miscellaneous Tasks

- Removed test file ([aac0688](https://github.com/atlas77-lang/Atlas77/commit/aac0688ec8319dab0309ad691ce0ee8f3110ccdd))

### Misc

- Cleaner pretty print of union literal ([5b5ed87](https://github.com/atlas77-lang/Atlas77/commit/5b5ed87d97252269ba2b8cd9bb1937c791d7fd42))
- Removed one fixed warning ([b3961ac](https://github.com/atlas77-lang/Atlas77/commit/b3961ac92cce17808fa146e341839efe61cc6441))
- Added a test for the upcoming `where` clause ([18b0929](https://github.com/atlas77-lang/Atlas77/commit/18b0929eb8330b433df16a84f2559399dc443fc6))
- Removed special case "_copy" method ([d9ce2a1](https://github.com/atlas77-lang/Atlas77/commit/d9ce2a10cba7c36689c1d4544846b999558e98ce))

# Changelog

All notable changes to this project will be documented in this file.

## [0.7.1] - 2026-01-09

### Bug Fixes

- Ownership & Typechecking Passes edge cases ([1a3a680](https://github.com/atlas77-lang/Atlas77/commit/1a3a68073ff2c16705dcdc3d294bed19f8e8c00b))
- Non_copyable flag didn't work properly ([b8d7fc4](https://github.com/atlas77-lang/Atlas77/commit/b8d7fc42b6fde7f4be9cd55cf181f71cbdf5aabc))
- Recursive copy error wouldn't trigger in generic structs ([586ac91](https://github.com/atlas77-lang/Atlas77/commit/586ac91f851f04ab57f343a460c6c84b71cb3b7d))
- Small issue with type mismatch error display ([c1c2c21](https://github.com/atlas77-lang/Atlas77/commit/c1c2c21831ae8b315856254d9ea7e173471d26c6))
- Destructor cannot have parameters ([143a54d](https://github.com/atlas77-lang/Atlas77/commit/143a54d0d5e24747c89c71d1e3760f1f69a6ca78))
- Automatic constructor generation didn't generate a valid constructor ([4f5c441](https://github.com/atlas77-lang/Atlas77/commit/4f5c4413315f7d0cd8c3c5fd55a8c57645facde7))
- Issue with &const T ([52219bc](https://github.com/atlas77-lang/Atlas77/commit/52219bc51ca3bb0667b1de2329db26a73092c488))
- Typo in one error ([a92915a](https://github.com/atlas77-lang/Atlas77/commit/a92915a7e8ff383681324b17e19ac67b86299db3))
- Fixed 2 use after free errors ([a2e66de](https://github.com/atlas77-lang/Atlas77/commit/a2e66de04d30326dd4a734c76240231d789f2696))
- Would crash instead of return warnings for unsupported expr ([3e03336](https://github.com/atlas77-lang/Atlas77/commit/3e03336f932dfe0127deb580583c111f2fce4a3d))
- Constructor/Destructor names are now `struct_ctor`/`struct_dtor` ([86451e4](https://github.com/atlas77-lang/Atlas77/commit/86451e49d89ce0ba14284356f3fe19e1c60205ff))
- While loops now checks for moved value inside of their body ([7cc262f](https://github.com/atlas77-lang/Atlas77/commit/7cc262fad13280dc9248a42e43e48aaa11e4e152))
- Unions weren't printed properly ([b33d4cc](https://github.com/atlas77-lang/Atlas77/commit/b33d4cc94acf2db53707b5f451f1fd2bf6ecd4e0))
- Added a potentially moved error and fixed use after free in if/else branches ([0e2f32d](https://github.com/atlas77-lang/Atlas77/commit/0e2f32d78574a5bbf8e8ede3fd143a40c5d32dac))
- Fixed operator precedence in assignment e.g.: `*x = 2` ([e429ad1](https://github.com/atlas77-lang/Atlas77/commit/e429ad19b7725261adc934a99c488407891447a7))
- Fixed issue with reference coercion for static method call ([6d9e24d](https://github.com/atlas77-lang/Atlas77/commit/6d9e24d8b81522dba1bcc75ac9e7b2febd123aad))
- Generics in function weren't properly printed ([2a133f6](https://github.com/atlas77-lang/Atlas77/commit/2a133f6de0524f9c05fca6b2ffcc14186a2aadf0))

### Documentation

- Forgot to update a note ([9c872a2](https://github.com/atlas77-lang/Atlas77/commit/9c872a2bc07cd4a756dfbe4f219b0c628c27dd57))

### Features

- Unions must have at least 2 variants to be valid + Fix of issue with automatically generated destructor, it would try to delete unions which would lead to UB ([bf75131](https://github.com/atlas77-lang/Atlas77/commit/bf7513198825ea5437cd07dd27e5217eb7094f68))
- Detect cyclic depencies in structs ([0ff53b1](https://github.com/atlas77-lang/Atlas77/commit/0ff53b1375b17b1a374ba2549747221ac987ad1f))
- Added a warning for structs marked as `std::copyable` but the compiler can't generate a copy constructor ([c9eb0c9](https://github.com/atlas77-lang/Atlas77/commit/c9eb0c9d0d50620f877e42bd70a492ea06a3221e))
- Introduced `std::copyable` & `std::non_copyable` flags to add on top of structs as hints to the compiler ([85a5b5f](https://github.com/atlas77-lang/Atlas77/commit/85a5b5fcb9bca51c08ea2a95f96217ad828e9a16))
- Implemented the new copy constructor on all the necessary types of the std ([37bb202](https://github.com/atlas77-lang/Atlas77/commit/37bb202e6f76eee39bf1663f1a41a2274244deaa))
- Implemented the new copy constructor in the while pipeline ([a181b18](https://github.com/atlas77-lang/Atlas77/commit/a181b1820f33e8ad8b57ae96d0269f53727c4d40))
- Now supports new copy constructor ([1e98e84](https://github.com/atlas77-lang/Atlas77/commit/1e98e84ecd52d049dcc0918055dfdac354465a2b))
- Added `Map<T>.values()` & `Map<T>.keys()` ([f0ae48c](https://github.com/atlas77-lang/Atlas77/commit/f0ae48cca3bd857267ff9059cac13844ef06f971))
- LIR supports "Hello Atlas" now ([6f69bff](https://github.com/atlas77-lang/Atlas77/commit/6f69bffcb4a56158bac0f0a626b4b25549c82698))
- Introduction of the LIR ([eccf530](https://github.com/atlas77-lang/Atlas77/commit/eccf530f8b40e868d667d245ecfb715d1e6d1917))
- Added replace/swap in `std/mem` ([00805d2](https://github.com/atlas77-lang/Atlas77/commit/00805d2c1a7a8fef095129939dd854842db12f34))
- Expanded `Queue<T>`/`Vector<T>` API ([b1696a9](https://github.com/atlas77-lang/Atlas77/commit/b1696a92e3005d2609ab17bfcd08a2c5972987f3))
- Added parsing support for global constant (nothing else though) ([92e08c1](https://github.com/atlas77-lang/Atlas77/commit/92e08c18feecf9c6a78c31b12acac2c124aec53c))

### Miscellaneous Tasks

- Removed debug output ([27992b7](https://github.com/atlas77-lang/Atlas77/commit/27992b7aea1dec8ddc296f0ed2b751f76c63f92c))
- Cargo clippy ([151ad09](https://github.com/atlas77-lang/Atlas77/commit/151ad092132728e271c461a90905c3a11a0737cb))
- Cleaned up the code with the help of clippy ([913f9a3](https://github.com/atlas77-lang/Atlas77/commit/913f9a35279b2a7a688015f63a48831dfd46a5bb))

### Misc

- Cargo fmt ([2c21914](https://github.com/atlas77-lang/Atlas77/commit/2c2191495b49ecce7fd41baf6a889d40b89a3798))
- Added flags to pretty printer ([d531fd4](https://github.com/atlas77-lang/Atlas77/commit/d531fd41a8e8fa6931fc5896cc06ff079c3dd34f))
- Updated all examples/tests ([3a1667b](https://github.com/atlas77-lang/Atlas77/commit/3a1667b2740b0d38da510a0b6e5863897b747efd))
- Removed a bunch of useless tests ([abbe8da](https://github.com/atlas77-lang/Atlas77/commit/abbe8da808ecb37f27da0acb5f63da0dd305cd2c))
- Added a bigger ref_test ([a3998c9](https://github.com/atlas77-lang/Atlas77/commit/a3998c9867796fca51aeccc68f81ad3dc029c46e))
- Discovered a new error with the ownership pass ([54dd0f9](https://github.com/atlas77-lang/Atlas77/commit/54dd0f97420a8859b84419cb0422ddd3715cdfaa))
- The compiler will now produces output in ./build ([a9ba7fb](https://github.com/atlas77-lang/Atlas77/commit/a9ba7fb8496e81fa226c993b0fa5326e17fdefa0))
- Start of the new instruction set ([89a5665](https://github.com/atlas77-lang/Atlas77/commit/89a56653100d9a824ab851e146550ce72070838b))
- Better memory report it now lists the struct names ([7046d91](https://github.com/atlas77-lang/Atlas77/commit/7046d9193b685fe8f477ac11dbb798d7d04203d0))

# Changelog

All notable changes to this project will be documented in this file.

## [0.7.0] - 2026-01-07

### Bug Fixes

- Well, I forgot to put a comma after the "this" parameter ([e6290ed](https://github.com/atlas77-lang/Atlas77/commit/e6290ed2f7c72c603d27c0e84e81c3d4d6c89567))
- Operator precedence with `*`/`&`/`-` ([6888675](https://github.com/atlas77-lang/Atlas77/commit/68886752f247d30024f6381ca126eb48c7897d16))
- Temporary fix for `optional<T>.value()`, it would drop the value too early ([8e77249](https://github.com/atlas77-lang/Atlas77/commit/8e77249a3e374e0d8898455fd212cf245bfb4f6d))
- Calling the copy constructor of a union variant would crash ([d3258bc](https://github.com/atlas77-lang/Atlas77/commit/d3258bc852409644590b64efde96f8dace02e4b9))
- Arrays of generic ty wouldn't have the inner ty name mangled ([d6d9486](https://github.com/atlas77-lang/Atlas77/commit/d6d9486274fcd716995c2cd0e92849165af9ca85))
- Removed "protected" keyword (wtf was it there?) ([bd842cb](https://github.com/atlas77-lang/Atlas77/commit/bd842cb22dd95309678e2ff5a77f3a3c46c8ae6a))
- Issue with references (&T would not be coerced to &const T in some cases) ([87e7e33](https://github.com/atlas77-lang/Atlas77/commit/87e7e33267160ed96c80bf232086ac69da748586))
- Added escape characters in string literals ([003a786](https://github.com/atlas77-lang/Atlas77/commit/003a786d717851060021b384dd1dbf8bd2e81e3e))
- Last use is a move is more often correctly used ([d8faeed](https://github.com/atlas77-lang/Atlas77/commit/d8faeed0e17336007ab188951fc338045160ae6e))
- Issues with Deref unary ([2e2b90f](https://github.com/atlas77-lang/Atlas77/commit/2e2b90f005650e3c04edc2566c59599ea9ec0c15))
- Every method on Vector<T> should now be stable-ish ([4f588d4](https://github.com/atlas77-lang/Atlas77/commit/4f588d4687b30cfbcf47561d30d7b88b6c6c41ca))
- Removed clunky test to see if we can move out of a container or not ([88d3896](https://github.com/atlas77-lang/Atlas77/commit/88d389634678d85d661597131d2007ed52851aa7))
- Generics struct weren't properly checked ([10c75fd](https://github.com/atlas77-lang/Atlas77/commit/10c75fded7a1c579a970b52222c5ef684df9dd0b))
- Issues with returning references would sometimes tries to move the value anyway ([e6cd147](https://github.com/atlas77-lang/Atlas77/commit/e6cd1479cc4b4fad5f8442d2cd88965db7514c50))
- Halt instruction wasn't CAPITALIZED ([36291b3](https://github.com/atlas77-lang/Atlas77/commit/36291b3cd34673ccd2a26c56c539a90de15c963a))
- BlueEngine builds again now (it still doesn't work, but it builds) ([a3ece3a](https://github.com/atlas77-lang/Atlas77/commit/a3ece3a5d7344666bf3865d08bad5fe0cc37d52d))
- Issues with deref operator not properly consuming ownership in some cases ([2332500](https://github.com/atlas77-lang/Atlas77/commit/2332500a71e6d5b574b96764379d24729e8dc032))
- Copy/Move expressions now create a tmp_var slot when being referenced ([b359309](https://github.com/atlas77-lang/Atlas77/commit/b3593097693bc78b39c918587fd27b066c2bbe3f))
- Remove the warning from deleting ref. They are now just ignored and you can use `delete_from_ref(&T) instead` ([4a25056](https://github.com/atlas77-lang/Atlas77/commit/4a250569670e8089ceed1237da021b0f94a843b9))
- Vector<T> ownership made it impossible to have a `.take(i)` or `.pop()`, it's now fixed with the use of `memcpy<T>(&T) -> T` ([ae7c018](https://github.com/atlas77-lang/Atlas77/commit/ae7c018b38fd44dd670449440ccde11639bc22c8))
- Removed double free error by just ignoring it ([d915c65](https://github.com/atlas77-lang/Atlas77/commit/d915c656251d216ed486b645a4d06c65c5385f8a))
- More fixes, but man, every type should just be `std::copyable`, it would fix so much issues ([66ef510](https://github.com/atlas77-lang/Atlas77/commit/66ef510945530dc45b06bfd762300ca32474c261))
- Issues with temporary references ([6589444](https://github.com/atlas77-lang/Atlas77/commit/6589444202c4156af2ec5b881cecf2539fcc4b10))
- Missing '{' in src ([afecdf7](https://github.com/atlas77-lang/Atlas77/commit/afecdf7320f5447a52aeaa05d21431efa98728e0))
- Blocks not adding "delete" at the end of the scope ([2b27f61](https://github.com/atlas77-lang/Atlas77/commit/2b27f6104578efb11394305c168fec4db5f01231))
- Made optional/expected not relied on generic types to be copyable ([55c4f97](https://github.com/atlas77-lang/Atlas77/commit/55c4f9714468559f03da2e4ef06794b547ac5011))
- Cannot transfer ownership in borrowing method (e.g.: `&this`, `&const this`) ([9ce37e6](https://github.com/atlas77-lang/Atlas77/commit/9ce37e60eb580c3e01758bef3c9d0be19d788e43))
- Issue with dereference in const function ([76e9875](https://github.com/atlas77-lang/Atlas77/commit/76e9875746d23d1942c897f79bad60986ec139a0))
- Issue when using String.into_iter() ([189a2b7](https://github.com/atlas77-lang/Atlas77/commit/189a2b7f131baa74ee496a5a33ab1ef2994d2dcc))
- Prevent double-delete of explicitly deleted variables ([59d7573](https://github.com/atlas77-lang/Atlas77/commit/59d7573988d30b12621c794a1b9139aa076cae3e))
- Require explicit borrowing and make lists non-copyable ([8632024](https://github.com/atlas77-lang/Atlas77/commit/8632024be4950d57dec692a2b704f85bb443cf12))
- A lot of bug fixes ([8955870](https://github.com/atlas77-lang/Atlas77/commit/8955870c140f8c4f0a5b0a63edaa05ba0eca9d24))
- `string` is now a copy type ([6881278](https://github.com/atlas77-lang/Atlas77/commit/6881278f00380d6ee9a5e2f2db535f14992142bb))
- Fixed an issue with unary operator not operatoring on the correct operand ([54aff18](https://github.com/atlas77-lang/Atlas77/commit/54aff1888bbc791f5d3f095bed6fa21e9b93cb38))
- Functions took ownership of values but never freed it ([db17fcc](https://github.com/atlas77-lang/Atlas77/commit/db17fcc721e2eb2f659a3d1d1d5f279bc6dbf7d1))
- Destructors not being called properly ([a526ec9](https://github.com/atlas77-lang/Atlas77/commit/a526ec90af56799e409fcc06a923c5dc77bb96d1))
- It was possible to mutate constant ref with `*a = something` ([67c8beb](https://github.com/atlas77-lang/Atlas77/commit/67c8beb2e0c60e8278622e7ce6ee957d0b653803))
- Returning a struct with a temporary ref was still possible ([1a8db46](https://github.com/atlas77-lang/Atlas77/commit/1a8db46e47376a59673f37bf823ca62b107c2afb))

### Documentation

- More info in the Roadmap ([c09afe5](https://github.com/atlas77-lang/Atlas77/commit/c09afe518b02d68dc05b8394fdaa4840fa55cee6))
- Updated `ROADMAP.md` ([2915adf](https://github.com/atlas77-lang/Atlas77/commit/2915adf0b17d0c38df9f46bf8061960448e0fe55))
- Added ROADMAP.md ([11b089f](https://github.com/atlas77-lang/Atlas77/commit/11b089fe2251b811afcfe9ef101416563081f46a))
- Updated the README.md ([b1d8464](https://github.com/atlas77-lang/Atlas77/commit/b1d84649d5cbf32ecfd457eb73fc419f6bb415a9))

### Features

- Warning for method chains that returns object without consuming them ([36ef22d](https://github.com/atlas77-lang/Atlas77/commit/36ef22dc3bff43967c65f9672602e62f664554cf))
- Added a warning for temporary variables not freed. ([4e8d420](https://github.com/atlas77-lang/Atlas77/commit/4e8d420dadfb5d899196a338974133a75bb93905))
- Now the compiler will warn you if there is an uneeded copy ([bfaaa85](https://github.com/atlas77-lang/Atlas77/commit/bfaaa85aaf279ac55f9a09035587c5f4366a67a8))
- Added a basic `Queue<T>` with a fixed size for now ([aec4663](https://github.com/atlas77-lang/Atlas77/commit/aec4663b43c4f02dfbc6d85f06f26a6b91eb0b96))
- Ref are now deref in print/println/panic to display everything properly ([5fe0da7](https://github.com/atlas77-lang/Atlas77/commit/5fe0da7330db99e20cb5aef854a6d1c5459bec06))
- Added unions & extern fn to pretty print ([078a486](https://github.com/atlas77-lang/Atlas77/commit/078a486a1389a768ee065b96e9bfb7e94e348d0e))
- Pretty printer for atlas77 hir code ([5d238f8](https://github.com/atlas77-lang/Atlas77/commit/5d238f804aeff1924a68a41c4ab6bf8186dcee6e))
- Implement recursive copy detection in ownership pass and add example ([83e46ae](https://github.com/atlas77-lang/Atlas77/commit/83e46ae3ffa5633870c5fc004b04b3784ca67777))
- Phantom Pop bug; Destructor LocalSpace Bug ([04376c5](https://github.com/atlas77-lang/Atlas77/commit/04376c5955decbad857b3c7d8a5d60177f1f1a7f))
- Added ObjectKind info to `RuntimeError::InvalidObjectAccess` ([ea7ac29](https://github.com/atlas77-lang/Atlas77/commit/ea7ac2925aad4ad10f9c7da20a562b40e2ff6c9b))
- Better error handling for string access ([3296da7](https://github.com/atlas77-lang/Atlas77/commit/3296da70aef1e0aea866fa9114296747efc8c1d0))
- Enhance ownership and memory management with new indexing and reference handling ([b784ac3](https://github.com/atlas77-lang/Atlas77/commit/b784ac3a249c052734d73f6e8fd865dbaaf7381c))
- Introduced `std::memcpy(&T) -> T` to allow for shallow copy of objects ([fb10ec7](https://github.com/atlas77-lang/Atlas77/commit/fb10ec7b2e142e6b5b45097fb0a71c57c61a2752))
- Automatically generated copy constructor ([f0d14c3](https://github.com/atlas77-lang/Atlas77/commit/f0d14c30ff3bed698f4b0b95d06f699de640cced))
- Automatic addition of a _copy constructor in structs ([a7e59e6](https://github.com/atlas77-lang/Atlas77/commit/a7e59e63a80c0cbf6c5d6becc3d793f2dbba2c5f))
- Add &this mutable reference modifier for struct methods ([76a51ca](https://github.com/atlas77-lang/Atlas77/commit/76a51ca2fadd71be9733fc25069650a2556a2da7))
- Added a bit more errors for consistency ([172ca08](https://github.com/atlas77-lang/Atlas77/commit/172ca08d3689afe04ed26092685ead9bdbf8bc37))
- Well, MOVE/COPY semantics are there I think... Still need some tests though ([1953acf](https://github.com/atlas77-lang/Atlas77/commit/1953acff643da304067553a608dac315d1660bd8))
- References are now actually useful ([70d26a6](https://github.com/atlas77-lang/Atlas77/commit/70d26a66f7f31e360527d99febf419333a822eaf))

### Miscellaneous Tasks

- Bump to v0.7.0 Covenant ([d6d97ce](https://github.com/atlas77-lang/Atlas77/commit/d6d97ce21f0920731d3ebd7b1aa0bb43e051bbe4))
- Expanded test.atlas ([fba787f](https://github.com/atlas77-lang/Atlas77/commit/fba787fb748c8f9610bd2f5737b41d0a32f02ded))
- Added v0.7.x branch to github actions ([87abf90](https://github.com/atlas77-lang/Atlas77/commit/87abf908bed4a7a318255e5d8e9730f880017fb8))

### Misc

- Removed debug output ([1d9babd](https://github.com/atlas77-lang/Atlas77/commit/1d9babd304dd8f85b2ff9f584324fc39c1630de5))
- Changed tests ownership folder name ([85bb4b0](https://github.com/atlas77-lang/Atlas77/commit/85bb4b0424779f1b3a8e508beeecefd05e08a905))
- Added this signature to methods ([d743396](https://github.com/atlas77-lang/Atlas77/commit/d7433968edb1a3c1e19775c00f776bb85802eb05))
- Updated clap & bumpalo to latest ([34e109e](https://github.com/atlas77-lang/Atlas77/commit/34e109eb37c6a80caeef1ca3b4d2e8cbfdfcaee6))
- Ref.atlas -> test2.atlas ([b1aabcb](https://github.com/atlas77-lang/Atlas77/commit/b1aabcb6bb9f84e6cdf649a905ed3707430b1592))
- Removed LifetimePass as everything has been moved to OwnershipPass ([17c30d0](https://github.com/atlas77-lang/Atlas77/commit/17c30d0bde8e788bb880cd84a49ce020be89ab56))

# Changelog

All notable changes to this project will be documented in this file.

## [0.6.4] - 2025-12-23

### Bug Fixes

- Fixed an edge case where sometimes non instantiated generics would still be registered. Also fixed deleting reference to primitives would throw an error when it should not ([5285ef3](https://github.com/atlas77-lang/Atlas77/commit/5285ef3734464180f821878b88eacf3ea7a7443e))
- Unions weren't properly imported, both their body & signature ([95537cb](https://github.com/atlas77-lang/Atlas77/commit/95537cb281616f5b63c46647b1ae9cd722c37c7e))
- References implements `std::copyable` as they are just a pointer. ([d9e6271](https://github.com/atlas77-lang/Atlas77/commit/d9e6271e485f23fa3585a4e1d938a2f33cff55b8))
- Removed runtime lib now useless because of Option/Result removal ([c9735e4](https://github.com/atlas77-lang/Atlas77/commit/c9735e4a69bf193fa4f98d6c293d28e2ae69ff3b))
- Fixed issue with generics object in parameters not being monomorphized correctly ([4a10cf4](https://github.com/atlas77-lang/Atlas77/commit/4a10cf4814a75a1c51c0ac7ca4bbc364aa175d4d))
- Removed `std::copyable` constraints from `optional<T>` ([f3fb34e](https://github.com/atlas77-lang/Atlas77/commit/f3fb34e6159997e38147a465c528390c18054d3e))
- Fixed String.find(sub_string) function ([155b06d](https://github.com/atlas77-lang/Atlas77/commit/155b06d3afbbc73a9b849e8d2e152c9b72c2b471))
- Fixed the error reporting not poiting to the constraint properly ([edaec2b](https://github.com/atlas77-lang/Atlas77/commit/edaec2bbc0fc23186f43300aa4aff5865146f149))
- Fixed error reporting for constraints ([e0ed349](https://github.com/atlas77-lang/Atlas77/commit/e0ed349910ac67df4a0caa0c4a5e0b8f9a28944b))
- Fixed an edge case where a generic type would be registered and constraints would not be checked ([e662170](https://github.com/atlas77-lang/Atlas77/commit/e6621707d794b434421d21f12e8bc36e99ef55c4))

### Documentation

- Fixed README.md typos ([33a0889](https://github.com/atlas77-lang/Atlas77/commit/33a0889007a9f3a3abf0a0eba0b0fb7bb1845f6b))

### Features

- Added full support for generic functions. ([b1288ff](https://github.com/atlas77-lang/Atlas77/commit/b1288ff6eb5bc71651391b0e0859ac2a05ecab64))
- Now function & extern fn are parsed and lowered with generic constraints ([250c0d4](https://github.com/atlas77-lang/Atlas77/commit/250c0d49923d27c93708a6a8304e3cf2ea0eb3eb))
- Start of the function generics ([6eaa9ce](https://github.com/atlas77-lang/Atlas77/commit/6eaa9ce9ee42a14d0d5558dd59aeed41ebb4c69b))
- Improved `expected<T, E>` and removed every usage of `Option<T>` in favour of `optional<T>` ([c90baed](https://github.com/atlas77-lang/Atlas77/commit/c90baedeeeade262b8e677d6c29b999465babbe7))
- Correctly checks the copy constructor signature. ([b481306](https://github.com/atlas77-lang/Atlas77/commit/b48130638caa0359189e6c61f3a91f0b0edb3d66))
- Added checks for `std::copyable` constraint ([76c369a](https://github.com/atlas77-lang/Atlas77/commit/76c369a90937f9bdb248ece7447f2f5092bb4a25))

### Miscellaneous Tasks

- Did what clippy wanted as always ([e5d2a4e](https://github.com/atlas77-lang/Atlas77/commit/e5d2a4e7fe03c22616d9689a3d45d46e2b800376))
- Removed Option/Result and replace them with optional/expected ([402ca24](https://github.com/atlas77-lang/Atlas77/commit/402ca24126f5fb2d895246e58bacaba410d51c88))

### Misc

- Added more std/experimental modules. I wanna do some tests ([b311493](https://github.com/atlas77-lang/Atlas77/commit/b31149382a54d4d95566b5584b0556f469e7d866))
- Remove the test function in `std/experimental/expected` ([c41c359](https://github.com/atlas77-lang/Atlas77/commit/c41c3593d2e3a83178925af37c614574156e01e7))
- Cleaned up code ([6e5cd21](https://github.com/atlas77-lang/Atlas77/commit/6e5cd2108cdbf643bda59a9ee79b8ef2b9339269))

# Changelog

All notable changes to this project will be documented in this file.

## [0.6.3] - 2025-12-16

### Bug Fixes

- A bit more fixes on the experimental std ([d4a0374](https://github.com/atlas77-lang/Atlas77/commit/d4a037421d15d8275f389c498c7a7e2ce92c9f0c))
- Fixed issue with char comparison ([55999ad](https://github.com/atlas77-lang/Atlas77/commit/55999ad9482ee7665ee6ac2b74d1d3c481452b42))
- Fixed missing std features & the test.atlas file itself ([a2b0b1b](https://github.com/atlas77-lang/Atlas77/commit/a2b0b1b1a20fdfa81ba04fa1cfbb3160c65ad9ab))
- Worked a bit more on the `optional<T>` syntax ([f8cadda](https://github.com/atlas77-lang/Atlas77/commit/f8caddad024fb7f985066f733b05b56cc5c1a2c9))
- Fix the `examples/README.md` file ([636c9f2](https://github.com/atlas77-lang/Atlas77/commit/636c9f24937591a9dfd978ba3febe523456be166))

### Features

- Unions finally work from end to end ([0aa1e91](https://github.com/atlas77-lang/Atlas77/commit/0aa1e914ca4447a442aa6a12109b9973d57acb66))
- Added unions up to the typechecker & monomorphization ([bbe58e5](https://github.com/atlas77-lang/Atlas77/commit/bbe58e56baec641618026444d6c45a5bc190ed3b))
- Added support for function type `(args_ty, ...) -> ret_ty` ([49b280e](https://github.com/atlas77-lang/Atlas77/commit/49b280e4552af5a38ca0545566610a643c0cec78))
- Added simple typechecking for union variants access ([0a927ce](https://github.com/atlas77-lang/Atlas77/commit/0a927cebb72ea866ef84db06c8fb5c0dcae35d11))
- Added union up until lowering in the sema ([f8ad4ba](https://github.com/atlas77-lang/Atlas77/commit/f8ad4ba481fc87770541beb5deaa3c0fd055ad28))
- Added `expected<T, E>` in the std/experimental ([56cecd0](https://github.com/atlas77-lang/Atlas77/commit/56cecd0982a11cb5fe5ba67e56764a17eeb8c1ac))
- Added syntax support for unions. ([ac467e7](https://github.com/atlas77-lang/Atlas77/commit/ac467e7f5acd026ba4b8701eeda3813bd1d982bf))
- Added `Map<K, V>.into_iter() -> Iter<Pair<K, V>>` ([f7bca2f](https://github.com/atlas77-lang/Atlas77/commit/f7bca2fd3f96c065eb29459f43c8e36a427995bc))

### Miscellaneous Tasks

- Bumped version from 0.6.2 to 0.6.3 ([6b09279](https://github.com/atlas77-lang/Atlas77/commit/6b09279642feadad4d81c86d06f130d61c5fd704))

### Refactor

- Reworked how to have uint64 literals (from `1_uint64` to `1u`) ([b4c67e6](https://github.com/atlas77-lang/Atlas77/commit/b4c67e626e069fc1c57de3b38f271d15a2a1da2e))

### Misc

- Applied Clippy changes ([1a4a545](https://github.com/atlas77-lang/Atlas77/commit/1a4a5453046d69879edcacc959e5dda8246e19b6))
- Added some more necessary structs to the upcoming dead code elimination ([e875ade](https://github.com/atlas77-lang/Atlas77/commit/e875ade6bbb78dd08bde0f8104a0c4f8b8e8d26c))
- Added v0.6.x & dev branch to PR checks ([768973b](https://github.com/atlas77-lang/Atlas77/commit/768973b6be7ab374a0775830f4bc8852f9c1917d))
- Removed `if_else` example ([df0999d](https://github.com/atlas77-lang/Atlas77/commit/df0999db04d41e769de1474bcac8aaf767b85cb2))
- A bit more work on the LIR. ([b6c69b9](https://github.com/atlas77-lang/Atlas77/commit/b6c69b91a93b4e3e7d3469d4597596bf049bf6c4))

# Changelog

All notable changes to this project will be documented in this file.

## [0.6.2] - 2025-12-13

### Bug Fixes

- Fixed issue for static access to generic type. e.g.: `Vector<T>::with_cap()` ([cb627a2](https://github.com/atlas77-lang/Atlas77/commit/cb627a2c494e88d0c65669ae4a5aabce95c474e4))
- Cleaner way of handling delete reference to primitive ([a5d1fc2](https://github.com/atlas77-lang/Atlas77/commit/a5d1fc28c642832e2848c1919f14413d3357ffd7))
- Added a check for the `DELETE_OBJ` instruction to avoid crashing when deleting reference to primitive ([5dfb8aa](https://github.com/atlas77-lang/Atlas77/commit/5dfb8aa766f0a2fc9c60df5fdc2396c7afcb877d))
- Added error reporting for illegal function call. ([1621d06](https://github.com/atlas77-lang/Atlas77/commit/1621d0623f46a36e772462531bb35b0ed9d9af2f))

### Features

- Experimental addition of `map<U>()` syntax ([57ffd1d](https://github.com/atlas77-lang/Atlas77/commit/57ffd1da78a6ee2bac446b7ac522f7310c9dc2f5))
- Added `std/experimental/` library ([9eb5657](https://github.com/atlas77-lang/Atlas77/commit/9eb565792314d7aaa515412f620aed7fcf0a2715))
- Added a way to chain field access (e.g.: `new Point(5, 5).x`) ([b97391a](https://github.com/atlas77-lang/Atlas77/commit/b97391a8e20d8e189c07de2b7607655bd8b703d3))
- Added grouping with `(expr)` syntax ([e8dfa5f](https://github.com/atlas77-lang/Atlas77/commit/e8dfa5f36640c95c5210333dd30036174131c8a2))

### Vector<T>

- :with_capacity(1) still doesn't work sadly, I'll try to fix asap ([472c630](https://github.com/atlas77-lang/Atlas77/commit/472c630de7f4663538f1c67e8a27ee8a57f5ae64))

### Misc

- Removed all the unnecessary examples ([c427152](https://github.com/atlas77-lang/Atlas77/commit/c4271526d9696d3c893c5957c0079fec71a83cbb))
- Removed the `libraries/unstable_std` and moved everything in the `std/experimental` ([1202982](https://github.com/atlas77-lang/Atlas77/commit/120298294df552f4969911a5ca22e1ace0b4fa85))
- Move std/cast from std to unstable_std ([433937d](https://github.com/atlas77-lang/Atlas77/commit/433937d02e882d656a5aa8dea8e7e7278761b397))

## [0.6.1] - 2025-12-12

### Bug Fixes

- Issue when having generic references in extern function ([5a08ed6](https://github.com/atlas77-lang/Atlas77/commit/5a08ed66502be8ee9a667cd2780d03ec43cb40e7))
- You can now delete primitive types but it will do nothing ([cf8151a](https://github.com/atlas77-lang/Atlas77/commit/cf8151af34857518e1f9b41e3b3ddc4ba6fd6a76))

### Documentation

- Updated the README.md ([57330c2](https://github.com/atlas77-lang/Atlas77/commit/57330c24dccbb70313f72bdda9b0e9a071358d9e))

### Features

- Finally was able to get &T and &const T to work correctly. ([8218010](https://github.com/atlas77-lang/Atlas77/commit/8218010fbef21a1d3a7cd9bc8971b8d4074aeb72))
- Start of the work on the Low Level IR ([9404cab](https://github.com/atlas77-lang/Atlas77/commit/9404cab5e41e21e78fdd896bc68270b2968c0690))
- Added `Map::<K, V>.size()` method ([edd0238](https://github.com/atlas77-lang/Atlas77/commit/edd0238480f0cd23d7a6ab901c5d8303250075c4))
- Added pretty print for arrays ([0e4ac34](https://github.com/atlas77-lang/Atlas77/commit/0e4ac34e73f40db1531c2b3c05f75800f317e188))
- Fixed some issues in the std ([bf01ef1](https://github.com/atlas77-lang/Atlas77/commit/bf01ef1c041ce1d682ecc3723e76309e8de2a406))

## [0.6.0] - 2025-12-10

### Bug Fixes

- Std/string didn't have to_chars in the runtime ([00eb8e9](https://github.com/atlas77-lang/Atlas77/commit/00eb8e9fc597bb7b524abe34e32fcbc9cc7b05da))

### Features

- Map<K, V> got promoted to stable. ([850db14](https://github.com/atlas77-lang/Atlas77/commit/850db1455c6cd1bbe56ab7fddd10cc0492692707))
- Added `std/iter` ([34c89df](https://github.com/atlas77-lang/Atlas77/commit/34c89df324d543ddc59b00d174aa92c6509c1363))
- Fixed the atlas code and added/fixed all the features the engine will need ([8aa0f8c](https://github.com/atlas77-lang/Atlas77/commit/8aa0f8c2d30ea4ed785d4c9ae3ecf88be67367fb))

## [0.6.0] - 2025-12-10

### Features

- Map<K, V> got promoted to stable. ([850db14](https://github.com/atlas77-lang/Atlas77/commit/850db1455c6cd1bbe56ab7fddd10cc0492692707))
- Added `std/iter` ([34c89df](https://github.com/atlas77-lang/Atlas77/commit/34c89df324d543ddc59b00d174aa92c6509c1363))
- Fixed the atlas code and added/fixed all the features the engine will need ([8aa0f8c](https://github.com/atlas77-lang/Atlas77/commit/8aa0f8c2d30ea4ed785d4c9ae3ecf88be67367fb))

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

