Users of the CovidSIM interface can use this section to estimate the impact of public health policies based on a number of chosen parameters. These public health policies include isolating sick patients and imposing general contact reduction policies (quarantine measures).

We use the following formula to calculate the impact of case isolation on the transmission:
$$
\text{impact case isolation}=\frac{Cp*Dp+(1-Fsick*Fiso*Phome)*(Di+Cl*Dl)}{(Cp*Dp+Di+Cl*Dl)}
$$
We use the following formula to calculate the impact of contact reduction policies on the transmission according to the estimated R from the previous sections (i.e. the observed R):
$$
\text{impact contact reduction} = \frac{\text{observed R}}{R_0\text{(impact case isolation)}}
$$
We use the following formula to calculate the percentage of transmission decreased due to the general contact reduction policies:
$$
\text{% impact contact reduction} = \frac{R_0\text{(impact case isolation)} - \text{observed R}}{R_0\text{(impact case isolation)}}
$$
