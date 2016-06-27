/**
The MIT License (MIT)

Copyright (c) 2016 Sahil Dua ( sahildua2305 | http://sahildua.com )

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.*/

#include <iostream>
#include <algorithm>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <queue>
#include <stack>
using namespace std;
typedef long long ll;
#define DEBUG
#define mod 1000000007
#define pb push_back

int r2, c2, n, m;

bool dfs(vector<string> graph, int r, int c){
    //cout<<r<<" "<<c<<endl;
    if(graph[r][c] == 'X'){
        if(r==r2 && c==c2)
            return true;
        else
            return false;
    }
    else{
        graph[r][c] = 'X';
    }
    if(r>0){
        if(dfs(graph, r-1, c))
            return true;
    }
    if(c>0){
        if(dfs(graph, r, c-1))
            return true;
    }
    if(r<(n-1)){
        if(dfs(graph, r+1, c))
            return true;
    }
    if(c<(m-1)){
        if(dfs(graph, r, c+1))
            return true;
    }
    return false;
}

struct point{
    int r,c;
    point(int rr, int cc){
        r = rr;
        c = cc;
    }
};

stack<point> st;

// if(r>0){
//     if(dfs(graph, r-1, c))
//         return true;
// }
// if(c>0){
//     if(dfs(graph, r, c-1))
//         return true;
// }
// if(r<(n-1)){
//     if(dfs(graph, r+1, c))
//         return true;
// }
// if(c<(m-1)){
//     if(dfs(graph, r, c+1))
//         return true;
// }

bool search(vector<string> graph, int rr, int cc){
    point t;
    t.r=rr;
    t.c=cc;
    st.push(t);

    while(!st.empty()){
        point u = st.top();
        st.pop();
        int r = u.r, c = u.c;
        cout<<r<<" "<<c<<endl;
        if(graph[r][c]=='X'){
            if(r==r2 && c==c2)
                return true;
            return false;
        }
        else{
            graph[r][c] = 'X';
        }
        if(r>0){
            t.r=r-1;
            t.c=c;
            st.push(t);
        }
        if(c>0){
            t.r=r;
            t.c=c-1;
            st.push(t);
        }
        if(r<(n-1)){
            t.r=r+1;
            t.c=c;
            st.push(t);
        }
        if(c<(m-1)){
            t.r=r;
            t.c=c+1;
            st.push(t);
        }
    }
    return false;
}

int main(){
    ios::sync_with_stdio(false);
    #ifdef DEBUG
    freopen("input.txt", "r", stdin);
    #endif // DEBUG

    cin>>n>>m;
    string temp;
    vector<string> graph;
    for(int i=0;i<n;i++){
        cin>>temp;
        graph.pb(temp);
    }
    int r1,c1;
    cin>>r1>>c1;
    cin>>r2>>c2;
    r2--;
    c2--;
    r1--;
    c1--;
    graph[r1][c1] = '.';
    if(search(graph, r1, c1))
        cout<<"YES\n";
    else
        cout<<"NO\n";

    return 0;
}
