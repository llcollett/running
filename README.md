# Standardising running pace via Garmin

The MAF method devised by Phil Maffetone specifies a heart rate range in which runners can calculate (180-age beats as the upper limit) and keep to whilst running in order to maintain maximum aerobic function (MAF). The method states that tests should be undertaken, on the same course (in order to reduce variability and thus have an effective comparator), at regular intervals, in order to track progress. However, I decided that, why not standardise all the training runs, adjusting for possible difference in heart rate and slope, in order to contribute all runs to test runs, to see progress more regularly over time. 

### Set-up

Get running! Once you have tracked a few runs from your chosen device, download the .fit files and convert them to .csv.

```
install r-studio
```

### Code

Run code in the following order:

```
importData.R
paceModel.R
```

Edit 'importData.R' with the name of your .csv files.

## Built With

* [RStudio](http://www.rstudio.com/)

## Acknowledgements

* [Phil Maffetone](https://philmaffetone.com/)
