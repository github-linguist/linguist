type link_ptr = ^link;
     data_ptr = ^data; (* presumes that type 'data' is defined above *)
     link = record
              prev: link_ptr;
              next: link_ptr;
              data: data_ptr;
            end;
