---
tags:
  - armchar/json
  - advancement
---
# Advancement
	
+ [[Saga Advancement Visualised.canvas|Saga Advancement Visualised]] illustrates the top level flow when a saga is loaded and processed.
+ We have to distinguish between in-game and pre-game advancement.
+ Common inference
	+ `addInference` applies to both CharGen and InGame advancement
+ See [[CharGen (Pre-Game Advancement)]]

## In-Game Advancement May 2026

+ The advancement process is currently under review.
+ The pivotal module for in-game advancement is `ArM.Saga.Advancement`
	+ `advanceSaga` creates a list of `Saga` objects by advancing to each stage required by the saga file, by recursively applying `stepSaga`
	+ `stepSaga` advances the `Saga` one season, relying on a large number of constituent functions, mostly mapping functions from 
		+ `ArM.Character.InGame`
		+ `ArM.Covenant.InGame`
+ `stepSaga` comprises some steps which simply map over characters/covenants and some steps which consider all the entities jointly
	+ each step depends on the state and the current advancement being processed
	+ the current advancement is stored at the head of past advancements, and update when required
	+ the State includes virtues and flaws which may modify behaviour.
+ These three modules are fairly well-structured at the moment.
+ The process is elaborated in [[Joint Advancement]]

## Validating SQ and XP use

+ In game XP allowance is governed by two components
	+ Source Quality
	+ BonusXP
+ explicit Source Quality is optional in most cases
+ explicit BonusXP is required for fringe cases
+ implicit Source Quality can often be deduced
	+ Exposure - always 2
	+ Story - from Covenant Story
	+ Reading - from book
	+ Teaching - from teacher
+ implicit BonusXP is typically bonuses from virtues and flaws

### Source Quality Calculation

| Mode      | Base       | Virtue            | Other                               | Traits             |
| :-------- | ---------- | ----------------- | ----------------------------------- | ------------------ |
| Adventure | Covenant   | Independent study |                                     | Any (5xp limit)    |
| Practice  | Individual | Independent study |                                     | Any (Usually One)  |
| Teaching  | Teacher    | Apt Student       | Specialitiy, one/two students, lab? | Decided by Teacher |
| Training  | Teacher    | Apt Student       |                                     | One                |
| Reading   | Book       | Book Learner      |                                     | One + Folio        |
| Vis study | Die roll   | Free study        |                                     | One                |
| Exposure  | 2          |                   |                                     | Any                |

+ Reading
	+ Defined Book + Ability + Extra

+ Bonuses may be lists, including justifications
+ Validation - compare standard SQ to autocomputed SQ

### Study from Teacher

Teaching has not been considered in the implementation yet.

+ Joint advancement, as implemented in `Saga`''s `advance`
	+ Parse 1.  Compute providing characters
		+ Teacher SQ
		+ Possessions given away
	+ Parse 2.  Compute receivers
		+ Taught student SQ
			+ do we infer XP gain from SQ, or just validate?
		+ Possessions received
+ How do we augment the advancements using information from the covenant and other characters?
	+ SQ from teacher (other character)
		+ tricky!  Inter-dependency between advancements


## Advancement Object

The table is not up to date.
Currently we use `Augmented Advancement`  with an explicit and an inferred (augmented) advancement, each using the same `Advancement` Type. Additionally, the `Augmented Advancement` has  a list of `Validation` objects.

| Field          | Advancement     | Augmented          | Type              | Comment                                            |     |     |     |
| :------------- | :-------------- | :----------------- | :---------------- | :------------------------------------------------- | --- | --- | --- |
| mode           | `advMode`       | (copied)           | `AdvancementType` | mode of study                                      |     |     |     |
| season         | `advSeason`     |                    | `SeasonTime`      | season or development stage                        |     |     |     |
| years          | `advYears`      | `augYears`         | `Maybe Int`       | number of years advanced                           |     |     |     |
| narrative      | `advNarrative`  | N/A                | `[ String ]`      | narrative description of the activities            |     |     |     |
| comment        | `advComment`    | N/A                | `[ String ]`      | freeform description of the activities             |     |     |     |
| requires       | requires        | requires           | `[ HarmKey ]`     | possessions required for exclusive use             |     |     |     |
| readsBook      | `readsBook`     | N/A                | `[ HarmKey ]`     | books read                                         |     |     |     |
| bookRead       | N/A             | `bookRead`         | `[ Book ]`        | Book inferred from Key                             |     |     |     |
| SQ             | `sourceQuality` | `baseSQ`           | `Maybe XPType`    | Source Quality (SQ)                                |     |     |     |
| score cap      | `sourceCap`     | `scoreCap`         | `Maybe Int`       | advancement cap on abilities/arts                  |     |     |     |
| Bonus SQ       | `bonusSQ`       | `bonusSQ`          | `[ BonusSQ ]`     | Bonus to Source Quality (SQ)                       |     |     |     |
| trait changes  | `changes`       | `changes`          | `[ ProtoTrait ]`  | trait changes defined by player                    |     |     |     |
| SQ as teacher  |                 | `teacherSQ`        | `Maybe XPType`    | The SQ generated as teacher                        |     |     |     |
| Spell levels   |                 | `levelLimit`       | `Maybe Int`       | spell level allowance                              |     |     |     |
| Postprocessing | N/A             | `postProcessTrait` | `PostProcessor`   | Extra postprocessing for traits at the given stage |     |     |     |


## Other challenges

+ Give/Receive possessions
	+ tricky!  Inter-dependency between advancements
+ Add book to covenant library
	+ process characters first, then covenants
+ P/G advancement
	+ P/G season
	+ P/G year
	+ P/G part-year ?  20 xp and 10 xp

## Deprecated: StepAdvance

+ The `StepAdvance` class described below is removed
	+ `Character` and `Covenant` are members of the `StepAdvance` class which provides the following function
		+ nextAdv -> `AdvancementStep` object
		+ applyAdv -> advances, `AdvancementStep` object in and out
		+ completeAdv -> completes the `AdvancementStep` and returns the Object
	+ Other functions could be interleaved between the above functions
		+ possibly using lists of `AdvancementStep` objects