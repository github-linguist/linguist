import datetime;
import numpy;
import from sklearn.feature_extraction.text { TfidfVectorizer }
import from sklearn.metrics.pairwise { cosine_similarity }

glob vectorizer = TfidfVectorizer();

def search_tweets(query: str, tweet:str) -> int;

node Profile {
    has username: str = "";

    can update with update_profile entry;

    can get with get_profile entry;

    can follow with follow_request entry;

    can un_follow with un_follow_request entry;
}

obj TweetInfo {
    has username: str;
    has id: str;
    has content: str;
    has embedding: list;
    has likes: list;
    has comments: list;
}

node Tweet {
    has content: str;
    has embedding: list;
    has created_at: str = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S");

    can update with update_tweet exit;

    can delete with remove_tweet exit;

    can like_tweet with like_tweet entry;

    can remove_like with remove_like entry;

    can comment with comment_tweet entry;

    def get_info()-> TweetInfo;

    can get with load_feed entry;
}

node Comment {
    has content: str;

    can update with update_comment entry;

    can delete with remove_comment entry;
}

edge Follow {}

edge Like {}

edge Post {}

walker visit_profile {
    can visit_profile with `root entry;
}

walker update_profile(visit_profile) {
    has new_username: str;
}

walker get_profile(visit_profile) {}

walker load_user_profiles {
    obj __specs__ {
        static has auth: bool = False;
    }
    can load_profiles with `root entry;

    can report_profiles with exit;
}

walker follow_request {}

walker un_follow_request {}

walker create_tweet(visit_profile) {
    has content: str;

    can tweet with Profile entry;
}

walker update_tweet {
    has updated_content: str;
}

walker remove_tweet {}

walker like_tweet {}

walker remove_like {}

walker comment_tweet {
    has content: str;
}

walker update_comment {
    has updated_content: str;
}

walker remove_comment {}

walker load_feed(visit_profile) {
    has search_query: str = "";
    has results: list = [];

    can load with Profile entry;

    can report_feed with exit;

}