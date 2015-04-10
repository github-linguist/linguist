/*
 * Copyright (C) 2012 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "ip.rsh"

static rs_matrix4x4 Mat;

void init() {
    rsMatrixLoadIdentity(&Mat);
}

void setMatrix(rs_matrix4x4 m) {
    Mat = m;
}

uchar4 __attribute__((kernel)) root(uchar4 in) {
    float4 f = convert_float4(in);
    f = rsMatrixMultiply(&Mat, f);
    f = clamp(f, 0.f, 255.f);
    return convert_uchar4(f);
}

