
+ Advancement process is managed from Saga
	+ [[Saga Advancement Visualised.canvas|Saga Advancement Visualised]] illustrates the top level flow when a saga is loaded and processed.
	+ `advanceSaga` maps the advancement functions over `characters` and `covenants`
	+ `Character` and `Covenant` are members of the `StepAdvance` class which provides the following function
		+ nextAdv -> `AdvancementStep` object
		+ applyAdv -> advances, `AdvancementStep` object in and out
		+ completeAdv -> completes the `AdvancementStep` and returns the Object
	+ Other functions could be interleaved between the above functions
		+ possibly using lists of `AdvancementStep` objects
+ [ ] Review below - is it still correct?		
+ For each season, the `advance` function is called independently for each `Character` and each `Covenant`
	+ This is not fully developed for `Covenant` yet, for `Character` it
		+ First augments the advancement
			+ adding limits
			+ adding bonus
			+ inferring additional traits
			+ validation
		+ Then applies the advancement
	    + In both cases it depends on the State from previous season and Advancement from current season.
	        + The State includes virtues and flaws which may modify behaviour.
+ Joint advancement, as implemented in `Saga`''s `advance`
	+ Parse 1.  Compute providing characters
		+ Teacher SQ
		+ Possessions given away
	+ Parse 2.  Compute receivers
		+ Taught student SQ
			+ do we infer XP gain from SQ, or just validate?
		+ Possessions received
+ How do we augment the advancements using information from the covenant and other characters?
	+ Books from CovenantState
		+ pass joint state to advancement augmentation
	+ SQ from teacher (other character)
		+ tricky!  Inter-dependency between advancements
	+ Give/Receive possessions
		+ tricky!  Inter-dependency between advancements
	+ Add book to covenant library
		+ process characters first, then covenants

+ P/G advancement
	+ P/G season
	+ P/G year
	+ P/G part-year ?  20 xp and 10 xp

## Source Quality Calculation

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


## Advancement Object


| Field          | Advancement    | Augmented          | Type              | Comment                                            |     |     |     |
| :------------- | :------------- | :----------------- | :---------------- | :------------------------------------------------- | --- | --- | --- |
| mode           | `advMode`      |                    | `AdvancementType` | mode of study                                      |     |     |     |
| season         | `advSeason`    |                    | `SeasonTime`      | season or development stage                        |     |     |     |
| years          | `advYears`     | `augYears`         | `Maybe Int`       | number of years advanced                           |     |     |     |
| narrative      | `advNarrative` | N/A                | `[ String ]`      | narrative description of the activities            |     |     |     |
| comment        | `advComment`   | N/A                | `[ String ]`      | freeform description of the activities             |     |     |     |
| uses book      | `advUses`      | `bookUsed`         | [ String/Book ]   | Books used exclusively by the character            |     |     |     |
| SQ             | `advSQ`        | `baseSQ`           | `Maybe XPType`    | Source Quality (SQ)                                |     |     |     |
| score cap      | `advCap`       | `scoreCap`         | `Maybe Int`       | advancement cap on abilities/arts                  |     |     |     |
| Bonus SQ       | `advBonus`     |                    | `Maybe XPType`    | Bonus to Source Quality (SQ)                       |     |     |     |
| Bonus SQ       |                | `bonusSQ`          | `XPType`          | Bonus to Source Quality from Virtues and Flaws     |     |     |     |
| trait changes  | `advChanges`   | `inferredTraits`   | `[ ProtoTrait ]`  | trait changes defined by player                    |     |     |     |
| SQ as teacher  |                | `teacherSQ`        | `Maybe Int`       | The SQ generated as teacher                        |     |     |     |
| Spell levels   |                | `levelLimit`       | `Maybe Int`       | spell level allowance                              |     |     |     |
| XP spent       | N/A            | `spentXP`          | `Maybe XPType`    | Total XP spent on advancement                      |     |     |     |
| Validation     | N/A            | `validation`       | `[Validation]`    | Report from validation                             |     |     |     |
| Postprocessing | N/A            | `postProcessTrait` | `PostProcessor`   | Extra postprocessing for traits at the given stage |     |     |     |
| Advancement    |                | ``advancement`     | `Advancement`     | Base advancement as entered by the user            |     |     |     |

```
data Advancement = Advancement
     { advMode :: AdvancementType -- ^ mode of study
     , advSeason :: SeasonTime    -- ^ season or development stage
     , advYears :: Maybe Int      -- ^ number of years advanced
     , advNarrative :: [ String ] -- ^ narrative description of the activities
     , advComment :: [ String ]   -- ^ freeform description of the activities
     , advUses :: [ String ]      -- ^ Books used exclusively by the character
     , advSQ :: Maybe XPType      -- ^ Source Quality (SQ)
     , advCap :: Maybe Int        -- ^ Source Quality (SQ)
     , advBonus :: Maybe XPType   -- ^ Bonus to Source Quality (SQ)
     , advChanges :: [ ProtoTrait ]  -- ^ trait changes defined by player
     , advSpellLevels :: Maybe Int   -- ^ spell level allowance
     , advTeacherSQ :: Maybe XPType  -- ^ The SQ generated as teacher
     , advSpentXP  :: `Maybe XPType  -- ^ Total XP spent on advancement
     , advValidation :: [Validation] -- ^ Report from validation
     , advPostprocessTrait :: PostProcessor -- ^ Extra postprocessing for traits at the given stage
     }
   deriving (Eq,Generic,Show)
```
