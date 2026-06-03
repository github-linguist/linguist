module default {
  type Task {
    required property text -> str;
    required property completed -> bool{
      default := false;
    };
  }
};
