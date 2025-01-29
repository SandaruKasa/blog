+++
title = "Many Particle Ballet"
summary = "Continuation on statistical ensembles with Monte-Carlo simulation: solving many-particle problem and trying to tackle polymer chains."
tags = ["math", "chemistry", "physics"]
author = "OneLightYearIsNotAYear"

date = "2025-01-30"

math = true
+++

Welcome back! [Last time](../2024-07-11-particle-tango)
we have discussed the interaction of two particles on a lattice.

Now it's time to come back to this fabulous thing.
This time, the conditions have changed.
Imagine having an $L \times L$ lattice with $N$ particles in it.
They are attracted to each other in one way or another.
What properties can you observe there?
Can we see some 'real' physical things, such as melting or solidifying?
We'll use the Metroplis-Hastings algorithm again, now a more efficient one.
Code can be found on [github](https://github.com/HF-Enjoyer/many-particle-ballet), as usual.

![Picture: two graphs side-by-side, each depicting a 10 by 10 square lattice
with red points in lattices' vertices; the left graph is titled "Before MC"
and the points in it are distributed mostly uniformly throughout the lattice,
occasionally gathering in small groups or lines;
the right graph is titled "After MC", it has the same number of points,
but all of them are close to each other in one big ball-like shape.](before_after.png)

## Monte-Carlo redesigned

In our previous attempt we had two particles moving on a lattice in an intricate dance.
But they were hardcoded to be a two-particle system so that's not a good way to go.
Ilya suggested a very nice thing and from now on every calculation is done
using his implementation of Metropolis-Hastings algorithm.
You should really check out his github (link above),
you will even find an accurate MC solver for a wavefunction of Helium atom.

You can find and explanation of what Metropolis-Hastings algorithm does in the previous chapter,
we won't discuss it here.

Here's the simplification of the algorithm
1. Initiate the system, number of particles, the lattice size and the iteration count.
2. Generate and initial configuration of particles through random placing
	(use the set! so you don't have to worry about duplicates)
3. Start iterations:
	1. change the position of a random particle and make a decision:
		stay in the old configuration or go to a new one
	2. speed thing up!
		look for a totally random configuration and decide whether you want to go there ---
		this helps the simulation converge quicker (debatable)
4. You're great. Keep going like this until you reach the desired number of iterations.

You may say that the additions are very obvious and easy to implement, but they weren't for me.
So I find it fascinating that such a simple code can converge a bunch of particles scattered in space
into something that looks like a real crystal structure.

## Running systems

We can run our systems with different temperatures.
The original algorithm by Ilia was implemented with the inverse potential:
$$U(r)=-\frac{1}{r}$$
But this would make more sense if we wanted to have Coulombic interactions
from charges with different signs (which we don't yet).
So I've added the Lennard-Jones potential parametrized in such a way
that the depth of the well $\varepsilon=-5.0$,
the $\sigma$ parameter is set so as to have
$$r_{min}=2^{1/6}\sigma=1$$

Potential:
$$U(r)=4\varepsilon \left (\left(\frac{\sigma}{r}\right)^{12}-\left(\frac{\sigma}{r}\right)^6 \right )$$

Let's run our simulations for $8000$ iterations (for the speed of it)
for a range of temperatures of $0--4$ with the step $T = 0.2$ and see where it takes us.

## Melting

So if we look at what could happen during the melting process,
we would see a sudden shift in density (see example with the water below).
This is what happens when we have a first-order phase transition.
Or, it is rather a definition of one --- in the Ehrenfest classification.

![Graph captioned "Density of water (and ice) as a function of temperature. Note
maximum density of water at 4%C [sic]. (Data from Pauling 1953
and Hutchinson 1957: 204.)". The X axis is temperature in degrees Celsius,
the Y axis is density in grams per cubic centimeter.
At first density drop linearly from ~0.9174 at -4 to ~0.9170 at 0
(this part of the curve is titled "ice").
Then there's a vertical jump up to ~0.9997 at 0 (titled "Melting or Freezing"),
followed by a what resembles an upside-down parabola (titled "Water")
with its peek being almost 1.0000 at 4 (titled "Maximum density at 4°C").](density_t.gif)

Of course, we would need to have a large crystal to simulate our system accurately
and it would have to have better potentials than we do.
We would also need a way to calculate density on a grid, and I have no clue how to do that
(for example, what do we do if our system splits up into clusters?).

I have a solution of a kind.
Instead of density, we're going to be looking for the average distance between the particles.
Interesting enough things emerge there.

### Inverse potential

We start with the simplest of potentials.
It gives nice crystals at low temps, but can we see anything behind that?
Can we see any 'melting'?

The answer is both yes and no.
![A graph with unlabeled X & Y axes.
A smooth red line labeled "Sigmoid function" in the legend
goes from x=0.0, y≈2.25 to x=4.0, y≈4.5.
A jagged green line labeled "Average distance" follows
red's path, nearly matching it in the beginning and the end,
and deviating by no more than ~0.5 in both sides along the Y axis in the middle.](inverse_melting.png)

Since the true phase change should be a discontinuation in density, I thought it would be nice
to fit it with the sigmoid function which is like a step function (sometimes).
And we see that the inverse potential is kind of changing its density...
but not with the speed we want it to.
Since we work in dimensionless units, temperature of $4$ is a lot.

### Lennard-Jones

LJ is probably the most popular choice to simulate intermolecular interactions.
And it gives way nicer results (but takes longer to compute, though)
![A graph similar to the previous one, but with the green line following
the red line much more closely (except for one abrupt jump up away from the red line
and back down to it at around x=1.2).](LJ_melting.png)

This looks way better! It isn't a 'true' melting yet, but it's way more definitive in my opinion.
We could steal some force fields from molecular dynamics,
but it's getting heavier to compute and I don't think this should be the goal of the project.

There is a spike around $1.25$, and I've seen it many times in slightly different positions.
I believe what is happening is that system starts fluctuating heavily near the critical point
and we get these jagged lines.
But I'll be glad if someone has another explanation and can prove me wrong.

## Polymer chains

We've recently rolled out version `0.5.0` - NOW WITH POLYMERS.
The approach is pretty much the same, but we have the polymer chain instead of single particles.

Here the single-monomer move scheme was used.
Select a random piece of polymer chain,
then propose a random move and accept it if it's valid and approved by the MH algorithm.
There is also a pivot and a snake-like scheme.

![A graph with two square lattices side-by-side.
The left one is titled "Initial Configuration" and it depicts a polymer chain
as ~30 red points in the lattice vertices (captioned "monomers" in the legend),
all connected into a single chain by one-lattice-cell-long blue lines on the edges of the lattice
(captioned "Polymer Chain" in the legend). The chain is approximately C-shaped
usually going one step to the side, then one step forward,
then one step to other side and one step forward again,
but in some places it goes straight for a couple of "units."
The right lattice is titled "Final Configuration" and is similar to the previous one,
except now all the "monomer" points are next to each other in one continuous blob,
while the blue "polymer chain" goes through them in a shape similar to Hilbert Curve,
but less regular.](polymer_before-after.png)

One funny artifact I have not figured out yet --- polymer chains don't want to move at zero temp.
And it's really weird, since we have special fixes for that in the code.
This would take me some research to figure out yet.

I would like to melt some polymers but it'd be nicer to have more than one chain on the screen
and to look into the interaction of those
(also I ran into the bug in the code I don't know how to fix yet).
So we'll leave it at that and maybe I will update this post with new cool graphs.

## Final notes

This simple task, which hadn't left my mind for nearly two years,
proved to be a fun coding and learning experience.
It's amazing that we can see 'crystals' forming
and even something that resembles the actual melting process.
I mean, such simple rules give us such complex behavior - I guess, it's just like in real life.
