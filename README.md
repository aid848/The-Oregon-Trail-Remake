# The Oregon Trail Remake

A remake of the popular MECC computer game, The Oregon Trail, written in Haskell.

# Quick Start:

### Requires Gloss Graphics Library: https://hackage.haskell.org/package/gloss

```
cabal install gloss
```
or if that doesn't work

```
cabal install gloss --lib
```
then 

```
ghc Main
./Main
```

# What is The Oregon Trail?

The Oregon Trail is a text-based strategy video game developed by Don Rawitsch, Bill Heinemann, and Paul Dillenberger in 1971 and produced by the Minnesota Educational Computing Consortium (MECC) beginning in 1975. It was developed by the three as a computer game to teach school children about the realities of 19th-century pioneer life on the Oregon Trail. In the game, the player assumes the role of a wagon leader guiding a party of settlers from Independence, Missouri, to Oregon City, Oregon via a covered wagon in 1847. (from wikipedia: https://en.wikipedia.org/wiki/The_Oregon_Trail_(1971_video_game))

You can play a DOS version of the game here: https://archive.org/details/msdos_Oregon_Trail_The_1990

# What's Cool About This Remake?

Our remake is written 100% in Haskell. All text and images are rendered via the Gloss library. The backend is coded from scratch and features pseudo-random event generation and a custom map.

# Usage:

1. Download the appropriate binary release for your OS. Alternatively, clone this repo and build the game with `ghc Main`.
2. Start the game with `./Main`.

# Team:
| <img src="https://avatars.githubusercontent.com/u/33972075?s=400&u=d3801ae66aa065bd3fd800d8afde3f4d9e565f03&v=4" width="144" /> | <img src="https://avatars0.githubusercontent.com/u/38742521?s=460&v=4" width="144" /> | <img src="https://avatars.githubusercontent.com/u/19785166?s=400&v=4" width="144" /> |
| --- | --- | --- |
| [Aidan Frost](https://github.com/aid848) | [James Zang](https://github.com/jameszang) | [Connor Quigg](https://github.com/cquigg528)

# Attributions:
Bitmap images by Minnesota Educational Computing Consortium (MECC)