data{
  int<lower=1> plots;
  int<lower=1> years;
  int<lower=2> densityclasses;

  int<lower=1> n_classes_obs //number of class observations (# of plot * # years)
}
