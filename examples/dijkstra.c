/* Dijkstra's algorithm */

/* compile with
       clang -S -emit-llvm -O1 dijkstra.c
   to produce dijkstra.s
       clang -c -emit-llvm -O1 dijkstra.c
   to produce dijkstra.o
       clang -o dijkstra dijkstra.c
   to produce executable dijkstra
*/

#include <stdio.h>

typedef unsigned int dist_t;
typedef int node_t;
typedef struct { node_t neighbor; dist_t distance; } edge_t;

#define N 7
#define EDGES_END {-1,0}
unsigned int edge_indices[N] = {0,2,6,10,15,19,22};
edge_t edges[] =
  {
    {1,1},                     EDGES_END, /* edges of node 0 */
    {2,7},{3,9},{6,14},        EDGES_END, /* edges of node 1 */
    {1,7},{3,10},{4,15},       EDGES_END, /* edges of node 2 */
    {1,9},{2,10},{4,11},{6,2}, EDGES_END, /* edges of node 3 */
    {2,15},{3,11},{5,6},       EDGES_END, /* edges of node 4 */
    {4,6},{6,9},               EDGES_END, /* edges of node 5 */
    {1,14},{3,2},{5,9},        EDGES_END, /* edges of node 6 */
  };

#define INFINITY -1
dist_t dist[N] = {INFINITY,INFINITY,INFINITY,INFINITY,INFINITY,INFINITY,INFINITY};
node_t prev[N] = {-1,-1,-1,-1,-1,-1,-1};
int seen[N] = {0,0,0,0,0,0,0};

void d(node_t source) {
  int i;
//  for (i = 0; i<N; i++) {
//    dist[i] = INFINITY;
//    seen[i] = 0;
//    prev[i] = -1;
//  }
  dist[source] = 0;
  while (1) {
    int closest = -1;
    unsigned int closest_dist = INFINITY;
    for (i=0; i<N; i++) {
      if (!seen[i] && dist[i] < closest_dist) {
        closest = i;
        closest_dist = dist[i];
        seen[i] = 1;
      }
    }
    if (closest == -1) break;
    unsigned int edge_index = edge_indices[closest];
    node_t n;
    while ( (n = edges[edge_index].neighbor) >= 0) {
      dist_t maybe_closer = dist[closest]+edges[edge_index].distance;
      if (maybe_closer < dist[n]) {
        dist[n] = maybe_closer;
        prev[n] = closest;
      }
      edge_index++;
    }
  }
}

int main() {
  int i;
  node_t source = 0;
  node_t target = 5;
  d(source);
  for (i=0; i<N; i++) {
    printf("dist[%d] = %d\n",i,dist[i]);
  }
  printf("%d",target);
  i = prev[target];
  while (i>=0) {
    printf("<-%d",i);
    i = prev[i];
  }
  printf("\n");
  return 0;
}
