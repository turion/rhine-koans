# Structure

* The test should maybe show a green check mark
* Each Koan should have a Haddocked substructure
* If bug in HLS can't be solved, provide my own hie.yaml

## CI

* Check cabal outdated

# Content
## Basic

* stdin clock
    * char count per line
    * line count & total char count
    * but summing char counts doesn't count newlines, so need all 3
    * exception for eof & final summary
    * cat a file in the tool
    * secondly progress report
* using the current time
* behaviours: reusability across clocks
    * E.g. revisit some of the earlier koans and refactor them
* infer clock interval from component, or vice versa? if not possible, add type signature?

* Arrow notation
    * Repeat some of the key exercises with arrows again
    * Correct file that has the Arrows extension missing
    * Arrows for ClSF and for SN
    * Complicated thing that is too hard to express with combinators, supply all components, rewrite it with Arrow

* feedback, building up state, delay, sum
* exception handling for control flow

* common mistakes?
    * arrM vs arrMCl

## Theory track

* LiftClock
* Pure clocks
* time domains
* Rescale
* Writing your own clock
* Writing your own MonadSchedule
* Complicated clock errors
    * Order of components (including a RB) is not the same as order of clocks in Rhine, this gives a weird error that `HasClock cl '[]` cannot be satisfied
* Write your own backend to some library

## Data analysis track

* file clock
    * text file processing with progress bar
* CSV clock
* Test your own rhines and develop them test-driven (`rhine-quickcheck` etc.)
    * Not sure on which track this belongs
* Time series analysis primer

## Gloss

## Terminal

* snake clone

## wai or servant

* One possible goal might be a Haskell menti clone
* Another possible goal: Caching file server with interaction via StdinClock (invalidate cache, report current status, start/stop live or summary logging, quit)

## Cassava

## Bayes

Goal: Song detection ML

## Bayes wai

Goal: Song detection with web frontend, and admin console

# Presentation

* 10 minutes: Basic concepts of rhine & plan for today
* 80 minutes: Installation & do basic tutorial together
* 15 minutes: Wrap up for now
  * Menti (or similar): What went well, what was hard? What was the hardest Koan? What questions are still open for you?
    * Can I write a Menti clone with Rhine?
  * Where to go from here: Advanced, or different backends?
  * Break, maybe find groups, think about in which direction you want to go
