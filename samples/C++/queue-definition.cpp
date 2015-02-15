namespace rosettacode
{
  template<typename T> class queue
  {
  public:
    queue();
    ~queue();
    void push(T const& t);
    T pop();
    bool empty();
  private:
    void drop();
    struct node;
    node* head;
    node* tail;
  };

  template<typename T> struct queue<T>::node
  {
    T data;
    node* next;
    node(T const& t): data(t), next(0) {}
  };

  template<typename T>
   queue<T>::queue():
    head(0)
  {
  }

  template<typename T>
   inline void queue<T>::drop()
  {
    node* n = head;
    head = head->next;
    delete n;
  }

  template<typename T>
   queue<T>::~queue()
  {
    while (!empty())
      drop();
  }

  template<typename T>
   void queue<T>::push(T const& t)
  {
    node*& next = head? tail->next : head;
    next = new node(t);
    tail = next;
  }

  template<typename T>
   T queue<T>::pop()
  {
    T tmp = head->data;
    drop();
    return tmp;
  }

  template<typename T>
   bool queue<T>::empty()
  {
    return head == 0;
  }
}
