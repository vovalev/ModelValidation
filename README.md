# ModelValidation

## Accuracy Ratio

This project aims to calculate the Accuracy Ratio (AR) for quantitative and qualitative variables.


## Data

The data are provided in the `data` folder. 
Beware, that the raw initial data is not used. 
The new (transformed) dataset is used.

The differences are as follows:

- The types of the companies changed from Cyrillic letters to Latin: `G` for good companies (which have not defaulted), and `D` for defaulted companies
- The type of the company was separated from the name of the company and saved in a variable `type`
- The dot `.` instead of comma `,` is used to separate fractional parts of a number
- Unnecessary commas at the end of each line were deleted