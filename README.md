Dozenal Tablature Viewer
========================

- Paste a regular plain text tablature and then view it in the base twelve number system instead of the base ten number system.
- All the tablature information is stored in the URL, so you easily can bookmark and save your tabs. No more dealing with all them ad-heavy tablature sites :)

## What is the base twelve numerical system?
The [base twelve system](https://en.wikipedia.org/wiki/Duodecimal) (also called *dozenal* or *duodecimal* system) is a number system in which we count till twelve instead of ten, and have dedicated symbols for ten (↊, a turned two) and eleven (↋, a turned three).

Subsequent numbers in the dozenal system: 
- 1, 2, 3, 4, 5, 6, 6, 7, 8, 9, ↊, ↋, 10, 11, ...

Subsequent numbers in the decimal system: 
- 1, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, ...

## Why would you want this?
In the end it is just a matter of fun or personal preference, but you might like a base twelve better in circumstances where it better matches the domain you're dealing with.
In guitar tablature specifically, there are twelve notes (semitones) in each octave, so it makes sense to have the number `10` match the octave and have dedicated symbols for each note below the octave.
The idea is that once you get used to using the dozenal system for reading tablature you require less mental work when reading tablature.
But it's mostly for fun.

## How do you pronounce numbers in the dozenal system?
↊ and ↋ are usually just pronounced *ten* and *eleven* respectively.
10 is pronounced *do*, 11 *do one*, 12 *do two* and so on.
So five notes above the octave is called *do five*.
It's funny coincidence here that the octave note has been called *do* in [musical solfège](https://en.wikipedia.org/wiki/Solf%C3%A8ge) for centuries.
For more information about pronounciation read [more about the do-gro-mo system](https://en.wikipedia.org/wiki/Duodecimal#Do-gro-mo_system).

## Can I customize the color theme?
Yes you can. Install the *Stylish* browser extension and override the color CSS variables in [index.css](https://github.com/dznl/tabviewer/blob/main/index.css).

## I want to undo my changes after I've already saved it
Every saved version is added to your browser history.
You can navigate them by going back and forth just like you would on other websites.

## How do I keep my bookmark up-to-date when modifying a tab?
There are browser-extensions which can help you with this.
For firefox there is [Update Bookmark](https://addons.mozilla.org/en-US/firefox/addon/replace-bookmark/) which reduces the amount of clicks you have to do to update an existing bookmark.

## Feature roadmap
- Make sure the Initialize action is called when the fragment changes (either due to popstate or by navigating to a different tab manually)
- [Dynamic bookmarks support](https://github.com/DaniloNovakovic/chrome-dynamic-bookmarks/issues/63)
- Progress indication for longer actions such as save and share
- Autoscroll
- Dedicated version for chord sheets
