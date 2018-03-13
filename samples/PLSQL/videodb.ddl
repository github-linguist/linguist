CREATE TABLE users (
   user_name varchar2(40),
   first_name varchar2(40),
   last_name varchar2(40),
   email varchar2(40),
   password varchar2(40),
   created_date DATE,
   total_credits NUMBER,
   credit_change_date DATE,
   PRIMARY KEY (user_name)
);
/

CREATE TABLE users_videos (
   video_id NUMBER,
   video_name varchar2(40),
   user_name varchar2(40),
   description varchar2(512),
   upload_date DATE,
   PRIMARY KEY (video_id),
   CONSTRAINT "USERS_VIDEOS_FK1" FOREIGN KEY ("USER_NAME") REFERENCES "USERS"("USER_NAME")
);
/

create or replace procedure print_user_videos(
    p_user_name in users.user_name%type
)
AUTHID DEFINER
as
    type t_user_videos is table of users_videos%rowtype
        index by pls_integer;
    l_videos t_user_videos;
begin

    select *
    bulk collect into l_videos
    from users_videos
    where user_name = p_user_name;

    for i in 1..l_videos.COUNT
    loop

        dbms_output.put_line(l_videos(i).video_name);

    end loop;

end print_user_videos;
/
