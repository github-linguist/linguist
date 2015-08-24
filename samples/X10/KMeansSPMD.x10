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

import x10.io.Console;
import x10.io.File;
import x10.io.Marshal;
import x10.io.IOException;
import x10.util.OptionsParser;
import x10.util.Option;
import x10.util.Team;

/**
 * An SPMD formulation of KMeans.
 *
 * For a highly optimized and scalable version of this benchmark see
 * KMeans.x10 in the X10 Benchmarks (separate download from x10-lang.org)
 */
public class KMeansSPMD {

    public static def printClusters (clusters:Rail[Float], dims:long) {
        for (d in 0..(dims-1)) { 
            for (k in 0..(clusters.size/dims-1)) { 
                if (k>0)
                    Console.OUT.print(" ");
                Console.OUT.print(clusters(k*dims+d).toString());
            }
            Console.OUT.println();
        }
    }

    public static def main (args:Rail[String]) {here == Place.FIRST_PLACE } {

        val opts = new OptionsParser(args, [
            Option("q","quiet","just print time taken"),
            Option("v","verbose","print out each iteration"),
            Option("h","help","this information")
        ], [
            Option("p","points","location of data file"),
            Option("i","iterations","quit after this many iterations"),
            Option("c","clusters","number of clusters to find"),
            Option("d","dim","number of dimensions"),
            Option("s","slices","factor by which to oversubscribe computational resources"),
            Option("n","num","quantity of points")
        ]);
        if (opts.filteredArgs().size!=0L) {
            Console.ERR.println("Unexpected arguments: "+opts.filteredArgs());
            Console.ERR.println("Use -h or --help.");
            System.setExitCode(1n);
            return;
        }
        if (opts("-h")) {
            Console.OUT.println(opts.usage(""));
            return;
        }

        val fname = opts("-p", "points.dat");
        val num_clusters=opts("-c",4);
        val num_slices=opts("-s",1);
        val num_global_points=opts("-n", 2000);
        val iterations=opts("-i",50);
        val dim=opts("-d", 4);
        val verbose = opts("-v");
        val quiet = opts("-q");

        if (!quiet)
            Console.OUT.println("points: "+num_global_points+" clusters: "+num_clusters+" dim: "+dim);

        // file is dimension-major
        val file = new File(fname);
        val fr = file.openRead();
        val init_points = (long) => Float.fromIntBits(Marshal.INT.read(fr).reverseBytes());
        val num_file_points = (file.size() / dim / 4) as Int;
        val file_points = new Rail[Float](num_file_points*dim, init_points);

        val team = Team.WORLD;

        val num_slice_points = num_global_points / num_slices / Place.numPlaces();

        finish {
            for (h in Place.places()) at(h) async {
                var compute_time:Long = 0;
                var comm_time:Long = 0;
                var barrier_time:Long = 0;

                val host_clusters  = new Rail[Float](num_clusters*dim, (i:long)=>file_points(i));
                val host_cluster_counts = new Rail[Int](num_clusters);

                for (slice in 0..(num_slices-1)) {
                    // carve out local portion of points (point-major)
                    val offset = (slice*Place.numPlaces() + here.id) * num_slice_points;
                    if (verbose)
                        Console.OUT.println(h.toString()+" gets "+offset+" len "+num_slice_points);
                    val init = (i:long) => {
                        val p=i%num_slice_points;
                        val d=i/num_slice_points;
                        return file_points(offset+p+d*num_file_points);
                    };

                    // these are pretty big so allocate up front
                    val host_points = new Rail[Float](num_slice_points*dim, init);
                    val host_nearest = new Rail[Float](num_slice_points);

                    val start_time = System.currentTimeMillis();

                    barrier_time -= System.nanoTime();
                    team.barrier();
                    barrier_time += System.nanoTime();

                    main_loop: for (iter in 0..(iterations-1)) {

                        //if (offset==0) Console.OUT.println("Iteration: "+iter);

                        val old_clusters = new Rail[Float](host_clusters.size);
                        Rail.copy(host_clusters, 0L, old_clusters, 0L, host_clusters.size);

                        host_clusters.clear();
                        host_cluster_counts.clear();

                        compute_time -= System.nanoTime();
                        for (p in 0..(num_slice_points-1)) {
                            var closest:Long = -1;
                            var closest_dist:Float = Float.MAX_VALUE;
                            for (k in 0..(num_clusters-1)) { 
                                var dist : Float = 0;
                                for (d in 0..(dim-1)) { 
                                    val tmp = host_points(p+d*num_slice_points) - old_clusters(k*dim+d);
                                    dist += tmp * tmp;
                                }
                                if (dist < closest_dist) {
                                    closest_dist = dist;
                                    closest = k;
                                }
                            }
                            for (d in 0..(dim-1)) { 
                                host_clusters(closest*dim+d) += host_points(p+d*num_slice_points);
                            }
                            host_cluster_counts(closest)++;
                        }
                        compute_time += System.nanoTime();

                        comm_time -= System.nanoTime();
                        team.allreduce(host_clusters, 0L, host_clusters, 0L, host_clusters.size, Team.ADD);
                        team.allreduce(host_cluster_counts, 0L, host_cluster_counts, 0L, host_cluster_counts.size, Team.ADD);
                        comm_time += System.nanoTime();

                        for (k in 0..(num_clusters-1)) {
                            for (d in 0..(dim-1)) host_clusters(k*dim+d) /= host_cluster_counts(k);
                        }

                        if (offset==0 && verbose) {
                            Console.OUT.println("Iteration: "+iter);
                            printClusters(host_clusters,dim);
                        }

                        // TEST FOR CONVERGENCE
                        for (j in 0..(num_clusters*dim-1)) {
                            if (true/*||Math.abs(clusters_old(j)-host_clusters(j))>0.0001*/) continue main_loop;
                        }

                        break;

                    } // main_loop

                } // slice

                Console.OUT.printf("%d: computation %.3f s communication %.3f s (barrier %.3f s)\n", 
                    here.id, compute_time/1E9, comm_time/1E9, barrier_time/1E9);

                team.barrier();

                if (here.id == 0) {
                    Console.OUT.println("\nFinal results:");
                    printClusters(host_clusters,dim);
                }

            } // async

        } // finish

    }

}

// vim: shiftwidth=4:tabstop=4:expandtab
