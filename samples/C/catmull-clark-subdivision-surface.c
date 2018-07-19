vertex face_point(face f)
{
	int i;
	vertex v;

	if (!f->avg) {
		f->avg = vertex_new();
		foreach(i, v, f->v)
			if (!i) f->avg->pos = v->pos;
			else    vadd(f->avg->pos, v->pos);

		vdiv(f->avg->pos, len(f->v));
	}
	return f->avg;
}

#define hole_edge(e) (len(e->f)==1)
vertex edge_point(edge e)
{
	int i;
	face f;

	if (!e->e_pt) {
		e->e_pt = vertex_new();
		e->avg = e->v[0]->pos;
		vadd(e->avg, e->v[1]->pos);
		e->e_pt->pos = e->avg;

		if (!hole_edge(e)) {
			foreach (i, f, e->f)
				vadd(e->e_pt->pos, face_point(f)->pos);
			vdiv(e->e_pt->pos, 4);
		} else
			vdiv(e->e_pt->pos, 2);

		vdiv(e->avg, 2);
	}

	return e->e_pt;
}

#define hole_vertex(v) (len((v)->f) != len((v)->e))
vertex updated_point(vertex v)
{
	int i, n = 0;
	edge e;
	face f;
	coord_t sum = {0, 0, 0};

	if (v->v_new) return v->v_new;

	v->v_new = vertex_new();
	if (hole_vertex(v)) {
		v->v_new->pos = v->pos;
		foreach(i, e, v->e) {
			if (!hole_edge(e)) continue;
			vadd(v->v_new->pos, edge_point(e)->pos);
			n++;
		}
		vdiv(v->v_new->pos, n + 1);
	} else {
		n = len(v->f);
		foreach(i, f, v->f)
			vadd(sum, face_point(f)->pos);
		foreach(i, e, v->e)
			vmadd(sum, edge_point(e)->pos, 2, sum);
		vdiv(sum, n);
		vmadd(sum, v->pos, n - 3, sum);
		vdiv(sum, n);
		v->v_new->pos = sum;
	}

	return v->v_new;
}

model catmull(model m)
{
	int i, j, a, b, c, d;
	face f;
	vertex v, x;

	model nm = model_new();
	foreach (i, f, m->f) {
		foreach(j, v, f->v) {
			_get_idx(a, updated_point(v));
			_get_idx(b, edge_point(elem(f->e, (j + 1) % len(f->e))));
			_get_idx(c, face_point(f));
			_get_idx(d, edge_point(elem(f->e, j)));
			model_add_face(nm, 4, a, b, c, d);
		}
	}
	return nm;
}
