This R script generates a stimulus pool for the Graded Novelty Encoding Task.

Files will be placed in the folder from where the script is executed!

The script generates 5 stimuli to use as familiars ("prototype1-5"), 50 stimuli ("p1-2*") to use during the study sequence
and 50 distractors ("p3-4*") for the test phase.

An additional 5 stimuli ("prototype6-10") are generated, but not used in the original GNET paradigm. These are the basis for the fully novel category.

Naming scheme used in the stimulus generation is different from that reported in the manuscript detailing this task.

Manuscript			Stimulus generator
- fully novel			- diffarr (different arrangement)
- novel shapes and colors	- sarr (same arrangement)
- novel colors			- sarrsha (same arrangement and shape)
- novel shapes 			- sarrcol (same arrangement and color)
- novel color combinations	- sarrshacol (same arrangement shape and color)

The picture naming scheme is based on the codes above: pXcodeY, where X denotes the number of the prototype the stimulus is generated from,
and Y denotes the number of the stimulus (1-2 are used in the study session, while 3-4 are used during the test).

The stimulus generator creates a picturedata.csv containing all the data of the stimulus pool.

This picturedata.csv contains three vectors per stimulus.

The first cell in each row clarifies the data the row contains:
- picture name only - vector representing arrangement
- picture name + col - vector representing colors
- picture name + sha - vector representing shapes

The following columns contain numerical values representing the stimuli as detailed in the manuscript
(which at the time of this writing is expected to be published in Behavior Research Methods).



