# Brand_Monaghan_Walker_17

This is the data for 'The Changing Role of Sound-Symbolism for Small Versus Large Vocabularies' by Brand, Monaghan and Walker, 2017.
The data is in .csv format, filename 'data_B-M-W_CS_17.csv'.

The data contains 15 columns (first row = column names), and the data is in long format (i.e. one row does not mean one participant, it refers to an individual trial during the experiment)
Below is a description of each column's data:

**ExperimentName** - The name of the experiment that was run, e.g. spikeycons12_extracols refers to the medium vocabulary size

**Category** - The vocabulary size being tested during the experiment (ordered variable, Small < Medium < Large

**Subject** - Unique IDs for participants, e.g. Medium22 refers to participant 22 in the medium vocabulary size

**SPIKYCON** - This is the version of the experiment, there were 8 different versions of the experiment, each mapping a sound to a different shape, see Monaghan et al. 2012's method section for more details

**Block** - The block of the experiment, range 1-4

**con_inc** - Whether the presentation was congrunet (c) or incongruent (i)

**Picture1** - The picture presented to the left of the screen

**Picture2** - The picture presented to the right of the screen

**same_diff** - Whether the shapes presented were from different categories (d), e.g. round - angular, or from the same category (s), e.g. round - round

**Slide3.ACC** - The dependent variable of accuracy, this is binomial where 1 = correct, 0 = incorrect

**Slide3.RT** - The reaction time for the response

**typefoil** - The type of foil shape the sound is being mapped to, p = pointy/angular congruent, px = pointy/angular incongruent, r = round congruent, rx = round incongruent

**typetarget**  - The type of target shape the sound is being to, p = pointy/angular congruent, px = pointy/angular incongruent, r = round congruent, rx = round incongruent

**Sound** - The sound that was played, these are not written phonetically

_Additional files_

**Materials**
Contains auditory and visual stimuli used in the experiment.
There is also the .ebs2 E-Prime experiment files:
Small vocabulary size condition - spikeycons8_extracols.ebs2
Medium vocabulary size condition - spikeycons12_extracols.ebs2
Large vocabulary size condition - spikeycons16_extracols.ebs2

**Brand_Monaghan_Walker_17_CS_analysis.R**
R analysis script, there are full annotations in this file.

**Questionnaire_data_2.csv**
Data file for the questionnaire experiment, where we quantitatively show a sound-symbolic preference for rounded/spiky shapes and continuant/plosive non-words.

