typedef struct {
  enum { INT } type;
  union {
    int int_val;
  };
} foc_value;

int foc_value_equal(foc_value*, foc_value*);
