typedef struct oct_node_t oct_node_t, *oct_node;
struct oct_node_t{
	/* sum of all colors represented by this node. 64 bit in case of HUGE image */
	uint64_t r, g, b;
	int count, heap_idx;
	oct_node kids[8], parent;
	unsigned char n_kids, kid_idx, flags, depth;
};

/* cmp function that decides the ordering in the heap.  This is how we determine
   which octree node to fold next, the heart of the algorithm. */
inline int cmp_node(oct_node a, oct_node b)
{
	if (a->n_kids < b->n_kids) return -1;
	if (a->n_kids > b->n_kids) return 1;

	int ac = a->count * (1 + a->kid_idx) >> a->depth;
	int bc = b->count * (1 + b->kid_idx) >> b->depth;
	return ac < bc ? -1 : ac > bc;
}

/* adding a color triple to octree */
oct_node node_insert(oct_node root, unsigned char *pix)
{
#	define OCT_DEPTH 8
	/* 8: number of significant bits used for tree.  It's probably good enough
	   for most images to use a value of 5.  This affects how many nodes eventually
	   end up in the tree and heap, thus smaller values helps with both speed
	   and memory. */

	unsigned char i, bit, depth = 0;
	for (bit = 1 << 7; ++depth < OCT_DEPTH; bit >>= 1) {
		i = !!(pix[1] & bit) * 4 + !!(pix[0] & bit) * 2 + !!(pix[2] & bit);
		if (!root->kids[i])
			root->kids[i] = node_new(i, depth, root);

		root = root->kids[i];
	}

	root->r += pix[0];
	root->g += pix[1];
	root->b += pix[2];
	root->count++;
	return root;
}

/* remove a node in octree and add its count and colors to parent node. */
oct_node node_fold(oct_node p)
{
	if (p->n_kids) abort();
	oct_node q = p->parent;
	q->count += p->count;

	q->r += p->r;
	q->g += p->g;
	q->b += p->b;
	q->n_kids --;
	q->kids[p->kid_idx] = 0;
	return q;
}

/* traverse the octree just like construction, but this time we replace the pixel
   color with color stored in the tree node */
void color_replace(oct_node root, unsigned char *pix)
{
	unsigned char i, bit;

	for (bit = 1 << 7; bit; bit >>= 1) {
		i = !!(pix[1] & bit) * 4 + !!(pix[0] & bit) * 2 + !!(pix[2] & bit);
		if (!root->kids[i]) break;
		root = root->kids[i];
	}

	pix[0] = root->r;
	pix[1] = root->g;
	pix[2] = root->b;
}

/* Building an octree and keep leaf nodes in a bin heap.  Afterwards remove first node
   in heap and fold it into its parent node (which may now be added to heap), until heap
   contains required number of colors. */
void color_quant(image im, int n_colors)
{
	int i;
	unsigned char *pix = im->pix;
	node_heap heap = { 0, 0, 0 };

	oct_node root = node_new(0, 0, 0), got;
	for (i = 0; i < im->w * im->h; i++, pix += 3)
		heap_add(&heap, node_insert(root, pix));

	while (heap.n > n_colors + 1)
		heap_add(&heap, node_fold(pop_heap(&heap)));

	double c;
	for (i = 1; i < heap.n; i++) {
		got = heap.buf[i];
		c = got->count;
		got->r = got->r / c + .5;
		got->g = got->g / c + .5;
		got->b = got->b / c + .5;
		printf("%2d | %3llu %3llu %3llu (%d pixels)\n",
			i, got->r, got->g, got->b, got->count);
	}

	for (i = 0, pix = im->pix; i < im->w * im->h; i++, pix += 3)
		color_replace(root, pix);

	node_free();
	free(heap.buf);
}
