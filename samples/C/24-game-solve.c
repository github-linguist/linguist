#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define n_cards 4
#define solve_goal 24
#define max_digit 9

typedef struct { int num, denom; } frac_t, *frac;
typedef enum { C_NUM = 0, C_ADD, C_SUB, C_MUL, C_DIV } op_type;

typedef struct expr_t *expr;
typedef struct expr_t {
        op_type op;
        expr left, right;
        int value;
} expr_t;

void show_expr(expr e, op_type prec, int is_right)
{
        const char * op;
        switch(e->op) {
        case C_NUM:     printf("%d", e->value);
                        return;
        case C_ADD:     op = " + "; break;
        case C_SUB:     op = " - "; break;
        case C_MUL:     op = " x "; break;
        case C_DIV:     op = " / "; break;
        }

        if ((e->op == prec && is_right) || e->op < prec) printf("(");
        show_expr(e->left, e->op, 0);
        printf("%s", op);
        show_expr(e->right, e->op, 1);
        if ((e->op == prec && is_right) || e->op < prec) printf(")");
}

void eval_expr(expr e, frac f)
{
        frac_t left, right;
        if (e->op == C_NUM) {
                f->num = e->value;
                f->denom = 1;
                return;
        }
        eval_expr(e->left, &left);
        eval_expr(e->right, &right);
        switch (e->op) {
        case C_ADD:
                f->num = left.num * right.denom + left.denom * right.num;
                f->denom = left.denom * right.denom;
                return;
        case C_SUB:
                f->num = left.num * right.denom - left.denom * right.num;
                f->denom = left.denom * right.denom;
                return;
        case C_MUL:
                f->num = left.num * right.num;
                f->denom = left.denom * right.denom;
                return;
        case C_DIV:
                f->num = left.num * right.denom;
                f->denom = left.denom * right.num;
                return;
        default:
                fprintf(stderr, "Unknown op: %d\n", e->op);
                return;
        }
}
int solve(expr ex_in[], int len)
{
        int i, j;
        expr_t node;
        expr ex[n_cards];
        frac_t final;

        if (len == 1) {
                eval_expr(ex_in[0], &final);
                if (final.num == final.denom * solve_goal && final.denom) {
                        show_expr(ex_in[0], 0, 0);
                        return 1;
                }
                return 0;
        }

        for (i = 0; i < len - 1; i++) {
                for (j = i + 1; j < len; j++)
                        ex[j - 1] = ex_in[j];
                ex[i] = &node;
                for (j = i + 1; j < len; j++) {
                        node.left = ex_in[i];
                        node.right = ex_in[j];
                        for (node.op = C_ADD; node.op <= C_DIV; node.op++)
                                if (solve(ex, len - 1))
                                        return 1;

                        node.left = ex_in[j];
                        node.right = ex_in[i];
                        node.op = C_SUB;
                        if (solve(ex, len - 1)) return 1;
                        node.op = C_DIV;
                        if (solve(ex, len - 1)) return 1;

                        ex[j] = ex_in[j];
                }
                ex[i] = ex_in[i];
        }

        return 0;
}

int solve24(int n[])
{
        int i;
        expr_t ex[n_cards];
        expr   e[n_cards];
        for (i = 0; i < n_cards; i++) {
                e[i] = ex + i;
                ex[i].op = C_NUM;
                ex[i].left = ex[i].right = 0;
                ex[i].value = n[i];
        }
        return solve(e, n_cards);
}

int main()
{
        int i, j, n[] = { 3, 3, 8, 8, 9 };
        srand(time(0));

        for (j = 0; j < 10; j++) {
                for (i = 0; i < n_cards; i++) {
                        n[i] = 1 + (double) rand() * max_digit / RAND_MAX;
                        printf(" %d", n[i]);
                }
                printf(":  ");
                printf(solve24(n) ? "\n" : "No solution\n");
        }

        return 0;
}
