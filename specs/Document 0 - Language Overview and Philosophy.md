# Document 0 - Language Overview and Philosophy

## 0.1 Mission Statement
Well, mission impossible kind of thingy, idk man. But I want a cool language

## 0.2 Design Philosophy and Principles
- **Keep the core language tiny, make everything else userland.**
- **Safety opt-in: you pay for what you use.**

> [!Note]
> Safety opt-in shouldn't be taken in the same way C++ does it. It mostly means that you get access by default to both unsafe & safe features, but safe features are at least as convenient and easy to use as unsafe ones. The best case scenario would be to make the safe versions easier to use so people would go naturally in that direction.

## 0.3 Non-Goals
This language is mostly an hobby language for me. I've just come to a conclusion that I'd need to write specifications if I wanted to go the next step. meaning being able to use it for my own personnal projects. So the language will never aims to:

- Replace an existing language
- Becoming a standard or not even being used in production anywhere. (It would be cool but it's not a goal)
- Forcing backwards compatibility (I like fixing stuff from the foundation, so if something is broken, just create a new breaking version/branch and continue on your life.)

## 0.4 Target Domains
My goal would be:
* Being able to make a game engine then a game with it
* Having some kind of `std::graphics` with binding for raylib, opengl, vk, dx12, ...
* Being able to bootstrap it fully 
> hence why I am making a specification, so I can follow it when bootstrapping

## 0.5 Relationship to other languages
Atlas77 is obviously inspired by the major systems programming languages (C/++, Rust, Zig, ...) but also by Java, C#, Python, Javascript, be it for the good or bad things/practices.

## 0.6 Safety guarantees
I don't really know yet what will the language promises or not in term of safety. Most probably something akin to Zig, but tbf idk, it's very blurry even to me, so any tips would be appreciated. But I will never guarantee as much safety as Rust does (see the Design Philosophy and Principles category to know why)

## 0.7 Quid of Undefined Behaviour
Well... I hope to remove as much of it as possible. I hate UB ngl, but I know it's useful in some cases, so idk, but I do plan to define all possible behaviour if I can because I don't want to fall in the C/C++ pitfall in terms of UB.

## 0.8 Versionning and Stability Policy
The versionning works with the default `v<MAJOR>.<MINOR>.<PATCH>-PRE_RELEASE_TAG` system.

After 1.0 everything should be stable, but every new MAJOR versions will never guarantee backwards compatibility with previous ones. Also, between MINOR versions there will be potential breaking of compatibility for safety related issue, but I will try to always properly document why, what's changed, how to fix your code, ... though I can only promise for now. Nothing is set in stone. The PRE_RELEASE_TAG is only there for `alpha`/`beta`/`rc_<number>` for the versions that are not fully tested yet, but already kind of implement everything.

## 0.9 Conformance
Well, I don't even know if I'll be able to conform to my own standard lmao. But if anyone wants to have a go at it, be free to try.
