# Genetic-algorithm-walking-simulation
parameterizing gait then using GA to optimize stability and speed of gait(ImageMagick Display is necessary)

At first, ImageMagick Display is necessary to create animation (gif file). I created a robot to show the gait.

Using popular physics formula(F=ma, integral a(acceleration) about t(time) is v(velocity) and interal v about t is location.)
So if I know a(acceleration) of a material about time, then I can get it's location by using numerical integration. 
I use a 3 stage RunggeKutta method to carry out numerical integration. 

The key is parameterizing the gait. Gait is very complex behavior and there are so many types of gait. 
But I want to explain the gait with number. Therefore I made 5 parameters(height of pelvis,stride, height of gait(
the highest height when lift the foot),width of center of pelvis and foot, period of a step) in gait to digitize gait.

If I input the number of each parameter and carry out numerical integration then I can show you the robot's gait.
When numerical values are not proper, simulating robot falls down.

Using the GA to optimize stability and speed, I want to observed the robot(number of each 5 parameters)
which walks the farthest distance in 5 seconds.   
