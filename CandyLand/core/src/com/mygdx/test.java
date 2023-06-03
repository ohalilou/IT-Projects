package com.mygdx;


public class test {
    
    public static void main(String[] args) {
        long start = System.currentTimeMillis();    
        long elapsedTime = System.nanoTime() - start;
    }

    // exponential easing out - decelerating to zero velocity
    private double function(double t,double b,double c,double d) {
	    return c * ( -Math.pow( 2, -10 * t/d ) + 1 ) + b;
    }
}
