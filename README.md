# Bias in HR

Simulation to replicate an interesitng claim made in lecture 'Unconscious Bias @ Work | Google Ventures'. This was that very small differences in distribution of appraisal distribution between two  different groups (e.g. genders) can have a startlingly large cumulative effect along career paths. The basic modelling assumptions are:

<ul>
<li>
At time zero, genders are divided exactly 50-50 at all grades.
<li>
At each time step, a proportion of staff (the 'churn rate') at each grade move out of that grade independently of gender. 
<li>
They are replace only from the grade below - i.e. there is no external recruitment into grades above the most junior.
<li>
The mechanism for doing this is ranking of all staff within-grade by a score which is drawn from the 'appraisal' distribution for their group (i.e. gender).
</ul>

See <a href="https://www.youtube.com/watch?v=nLjFTHTgEVU&t=0s">Unconscious Bias @ Work | Google Ventures</a>.
