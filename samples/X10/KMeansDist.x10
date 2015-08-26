/*
 *  This file is part of the X10 project (http://x10-lang.org).
 *
 *  This file is licensed to You under the Eclipse Public License (EPL);
 *  You may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *      http://www.opensource.org/licenses/eclipse-1.0.php
 *
 *  (C) Copyright IBM Corporation 2006-2014.
 */

import x10.array.*;
import x10.io.Console;
import x10.util.Random;

/**
 * A low performance formulation of distributed KMeans using fine-grained asyncs.
 *
 * For a highly optimized and scalable, version of this benchmark see
 * KMeans.x10 in the X10 Benchmarks (separate download from x10-lang.org)
 */
public class KMeansDist {

    static val DIM=2;
    static val CLUSTERS=4;
    static val POINTS=2000;
    static val ITERATIONS=50;

    public static def main (Rail[String]) {
        val world = Place.places();
        val local_curr_clusters = 
            PlaceLocalHandle.make[Array_2[Float]](world, () => new Array_2[Float](CLUSTERS, DIM));
        val local_new_clusters = 
            PlaceLocalHandle.make[Array_2[Float]](world, () =>  new Array_2[Float](CLUSTERS, DIM));
        val local_cluster_counts = 
            PlaceLocalHandle.make[Rail[Int]](world, ()=> new Rail[Int](CLUSTERS));

        val rnd = PlaceLocalHandle.make[Random](world, () => new Random(0));
        val points = new DistArray_Block_2[Float](POINTS, DIM, world, (Long,Long)=>rnd().nextFloat());

        val central_clusters = new Array_2[Float](CLUSTERS, DIM, (i:Long, j:Long) => {
            at (points.place(i,j)) points(i,j)
        });

	val old_central_clusters = new Array_2[Float](CLUSTERS, DIM);

        val central_cluster_counts = new Rail[Int](CLUSTERS);

        for (iter in 1..ITERATIONS) {

            Console.OUT.println("Iteration: "+iter);

            finish {
                // reset state
                for (d in world) at (d) async {
                    for ([i,j] in central_clusters.indices()) {
                        local_curr_clusters()(i, j) = central_clusters(i, j);
                        local_new_clusters()(i, j) = 0f;
                    }
            
                    local_cluster_counts().clear();
                }
            }

            finish {
                // compute new clusters and counters
                for (p in 0..(POINTS-1)) {
                    at (points.place(p,0)) async {
                        var closest:Long = -1;
                        var closest_dist:Float = Float.MAX_VALUE;
                        for (k in 0..(CLUSTERS-1)) { 
                            var dist : Float = 0;
                            for (d in 0..(DIM-1)) { 
                                val tmp = points(p,d) - local_curr_clusters()(k, d);
                                dist += tmp * tmp;
                            }
                            if (dist < closest_dist) {
                                closest_dist = dist;
                                closest = k;
                            }
                        }
			atomic {
                            for (d in 0..(DIM-1)) { 
                                local_new_clusters()(closest,d) += points(p,d);
                            }
                            local_cluster_counts()(closest)++;
                        }
                    }
                }
            }

            for ([i,j] in old_central_clusters.indices()) {
                old_central_clusters(i, j) = central_clusters(i, j);
                central_clusters(i, j) = 0f;
            }
           
            central_cluster_counts.clear();

            finish {
                val central_clusters_gr = GlobalRef(central_clusters);
                val central_cluster_counts_gr = GlobalRef(central_cluster_counts);
                val there = here;
                for (d in world) at (d) async {
                    // access PlaceLocalHandles 'here' and then data will be captured by at and transfered to 'there' for accumulation
                    val tmp_new_clusters = local_new_clusters();
                    val tmp_cluster_counts = local_cluster_counts();
                    at (there) atomic {
                        for ([i,j] in tmp_new_clusters.indices()) {
                            central_clusters_gr()(i,j) += tmp_new_clusters(i,j);
                        }
                        for (j in 0..(CLUSTERS-1)) {
                            central_cluster_counts_gr()(j) += tmp_cluster_counts(j);
                        }
                    }
                }
            }

            for (k in 0..(CLUSTERS-1)) { 
                for (d in 0..(DIM-1)) { 
                    central_clusters(k, d) /= central_cluster_counts(k);
                }
            }

            // TEST FOR CONVERGENCE
            var b:Boolean = true;
            for ([i,j] in old_central_clusters.indices()) { 
                if (Math.abs(old_central_clusters(i, j)-central_clusters(i, j))>0.0001) {
                    b = false;
                    break;
                }
            }
            if (b) break;

        }

        for (d in 0..(DIM-1)) { 
            for (k in 0..(CLUSTERS-1)) { 
                if (k>0)
                    Console.OUT.print(" ");
                Console.OUT.print(central_clusters(k,d));
            }
            Console.OUT.println();
        }
    }
}

// vim: shiftwidth=4:tabstop=4:expandtab
