// Copyright 2015, Barefoot Networks, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

action set_mirror_id(session_id) {
    clone_ingress_pkt_to_egress(session_id);
}

table mirror_acl {
    reads {
        ingress_metadata.if_label : ternary;
        ingress_metadata.bd_label : ternary;

        /* ip acl */
        ingress_metadata.lkp_ipv4_sa : ternary;
        ingress_metadata.lkp_ipv4_da : ternary;
        ingress_metadata.lkp_ip_proto : ternary;

        /* mac acl */
        ingress_metadata.lkp_mac_sa : ternary;
        ingress_metadata.lkp_mac_da : ternary;
        ingress_metadata.lkp_mac_type : ternary;
    }
    actions {
        nop;
        set_mirror_id;
    }
    size : INGRESS_MIRROR_ACL_TABLE_SIZE;
}
