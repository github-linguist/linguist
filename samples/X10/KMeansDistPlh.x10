/*
 *  This file is part of the X10 project (http://x10-lang.org).
 *
 *  This file is licensed to You under the Eclipse Public License (EPL);
 *  You may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *      http://www.opensource.org/licenses/eclipse-1.0.php
 *
 *  (C) Copyright IBM Corporation 2006-2015.
 */

import x10.array.Array;
import x10.array.Array_2;
import x10.compiler.Foreach;
import x10.util.Random;

/**
 * A better formulation of distributed KMeans using coarse-grained asyncs to
 * implement an allreduce pattern for cluster centers and counts.
 *
 * For a highly optimized and scalable, version of this benchmark see
 * KMeans.x10 in the X10 Benchmarks (separate download from x10-lang.org)
 */
public class KMeansDistPlh {

    static val DIM=2;
    static val CLUSTERS=4;

    static class ClusterState {
        val clusters = new Array_2[Float](CLUSTERS, DIM);
        val clusterCounts = new Rail[Int](CLUSTERS);
    }

    public static def main(args:Rail[String]) {
        val numPoints = args.size > 0 ? Long.parse(args(0)) : 2000;
        val iterations = args.size > 1 ? Long.parse(args(1)) : 50;
        val world = Place.places();

        val clusterStatePlh = PlaceLocalHandle.make[ClusterState](world, () => new ClusterState());
        val currentClustersPlh = PlaceLocalHandle.make[Array_2[Float]](world, () => new Array_2[Float](CLUSTERS, DIM));
        val pointsPlh = PlaceLocalHandle.make[Array_2[Float]](world, () => {
            val rand = new Random(here.id);
            return new Array_2[Float](numPoints/world.size(), DIM, (Long,Long)=>rand.nextFloat());
        });

        val centralCurrentClusters = new Array_2[Float](CLUSTERS, DIM);
        val centralNewClusters = new Array_2[Float](CLUSTERS, DIM);
        val centralClusterCounts = new Rail[Int](CLUSTERS);

        // arbitrarily initialize central clusters to first few points
        for ([i,j] in centralCurrentClusters.indices()) {
            centralCurrentClusters(i,j) = pointsPlh()(i,j);
        }

        for (iter in 1..iterations) {
            Console.OUT.println("Iteration: "+iter);

            finish {
                for (place in world) async {
                    val placeClusters = at(place) {
                        val currentClusters = currentClustersPlh();
                        Array.copy(centralCurrentClusters, currentClusters);

                        val clusterState = clusterStatePlh();
                        val newClusters = clusterState.clusters;
                        newClusters.clear();
                        val clusterCounts = clusterState.clusterCounts;
                        clusterCounts.clear();

                        // compute new clusters and counters
                        val points = pointsPlh();

                        for (p in 0..(points.numElems_1-1)) {
                            var closest:Long = -1;
                            var closestDist:Float = Float.MAX_VALUE;
                            for (k in 0..(CLUSTERS-1)) { 
                                var dist : Float = 0;
                                for (d in 0..(DIM-1)) { 
                                    val tmp = points(p,d) - currentClusters(k, d);
                                    dist += tmp * tmp;
                                }
                                if (dist < closestDist) {
                                    closestDist = dist;
                                    closest = k;
                                }
                            }

                            atomic {
                                for (d in 0..(DIM-1)) { 
                                    newClusters(closest,d) += points(p,d);
                                }
                                clusterCounts(closest)++;
                            }
                        }
                        clusterState
                    };

                    // combine place clusters to central
                    atomic {
                        for ([i,j] in centralNewClusters.indices()) {
                            centralNewClusters(i,j) += placeClusters.clusters(i,j);
                        }
                        for (j in 0..(CLUSTERS-1)) {
                            centralClusterCounts(j) += placeClusters.clusterCounts(j);
                        }
                    }
                }
            }

            for (k in 0..(CLUSTERS-1)) { 
                for (d in 0..(DIM-1)) { 
                    centralNewClusters(k, d) /= centralClusterCounts(k);
                }
            }

            // TEST FOR CONVERGENCE
            var b:Boolean = true;
            for ([i,j] in centralCurrentClusters.indices()) { 
                if (Math.abs(centralCurrentClusters(i, j)-centralNewClusters(i, j)) > 0.0001) {
                    b = false;
                    break;
                }
            }

            Array.copy(centralNewClusters, centralCurrentClusters);

            if (b) break;

            centralNewClusters.clear();
            centralClusterCounts.clear();
        }

        for (d in 0..(DIM-1)) { 
            for (k in 0..(CLUSTERS-1)) { 
                if (k > 0)
                    Console.OUT.print(" ");
                Console.OUT.print(centralCurrentClusters(k,d));
            }
            Console.OUT.println();
        }
    }
}

// vim: shiftwidth=4:tabstop=4:expandtab
