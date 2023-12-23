# Structure

* Should be numbered, but also have a short title
* Every koan has a lib, an executable, and a test
* Separate branch where all koans are solved
* How to do koans for different backends? Separate packages? Or rather separate sublibraries, like basic, advanced etc.?

# Content
## Basic

* Running a program that outputs something every second
* Adjust time at type level
* Different interval for clock than for component
* infer clock interval from component, or vice versa? if not possible, add type signature?
* compose two clsfs
* can't compose two clsfs at different clocks: delete one of them
* want to compose two clsfs at different clocks: RB

## Advanced

* LiftClock
* Rescale
* Pure clocks
* Writing your own clock
* Writing your own MonadSchedule
* Complicated clock errors
    * Order of components (including a RB) is not the same as order of clocks in Rhine, this gives a weird error that `HasClock cl '[]` cannot be satisfied

## Gloss

## Terminal

## wai or servant

## Cassava

## Bayes

Goal: Song detection ML

## Bayes wai

Goal: Song detection with web frontend, and admin console
