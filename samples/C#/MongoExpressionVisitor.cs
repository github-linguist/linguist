using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Linq.Expressions;

namespace MongoDB.Linq.Expressions
{
    internal class MongoExpressionVisitor : ExpressionVisitor
    {
        protected override Expression Visit(Expression exp)
        {
            if (exp == null)
                return null;
            switch ((MongoExpressionType)exp.NodeType)
            {
                case MongoExpressionType.Collection:
                    return VisitCollection((CollectionExpression)exp);
                case MongoExpressionType.Field:
                    return VisitField((FieldExpression)exp);
                case MongoExpressionType.Projection:
                    return VisitProjection((ProjectionExpression)exp);
                case MongoExpressionType.Select:
                    return VisitSelect((SelectExpression)exp);
                case MongoExpressionType.Aggregate:
                    return VisitAggregate((AggregateExpression)exp);
                case MongoExpressionType.AggregateSubquery:
                    return VisitAggregateSubquery((AggregateSubqueryExpression)exp);
                case MongoExpressionType.Scalar:
                    return VisitScalar((ScalarExpression)exp);
                default:
                    return base.Visit(exp);
            }
        }

        protected virtual Expression VisitAggregate(AggregateExpression aggregate)
        {
            var exp = Visit(aggregate.Argument);
            if (exp != aggregate.Argument)
                return new AggregateExpression(aggregate.Type, aggregate.AggregateType, exp, aggregate.Distinct);

            return aggregate;
        }

        protected virtual Expression VisitAggregateSubquery(AggregateSubqueryExpression aggregateSubquery)
        {
            Expression e = Visit(aggregateSubquery.AggregateAsSubquery);
            ScalarExpression subquery = (ScalarExpression)e;
            if (subquery != aggregateSubquery.AggregateAsSubquery)
                return new AggregateSubqueryExpression(aggregateSubquery.GroupByAlias, aggregateSubquery.AggregateInGroupSelect, subquery);
            return aggregateSubquery;
        }

        protected virtual Expression VisitCollection(CollectionExpression collection)
        {
            return collection;
        }

        protected virtual Expression VisitField(FieldExpression field)
        {
            var e = Visit(field.Expression);
            if (field.Expression != e)
                field = new FieldExpression(e, field.Alias, field.Name);

            return field;
        }

        protected virtual Expression VisitProjection(ProjectionExpression projection)
        {
            var source = (SelectExpression)Visit(projection.Source);
            var projector = Visit(projection.Projector);
            if (source != projection.Source || projector != projection.Projector)
                return new ProjectionExpression(source, projector, projection.Aggregator);
            return projection;
        }

        protected ReadOnlyCollection<OrderExpression> VisitOrderBy(ReadOnlyCollection<OrderExpression> orderBys)
        {
            if (orderBys != null) 
            {
                List<OrderExpression> alternate = null;
                for (int i = 0, n = orderBys.Count; i < n; i++) 
                {
                    OrderExpression expr = orderBys[i];
                    Expression e = this.Visit(expr.Expression);
                    if (alternate == null && e != expr.Expression) 
                        alternate = orderBys.Take(i).ToList();
                    if (alternate != null) 
                        alternate.Add(new OrderExpression(expr.OrderType, e));
                }
                if (alternate != null) 
                    return alternate.AsReadOnly();
            }
            return orderBys;
        }

        protected virtual Expression VisitScalar(ScalarExpression scalar)
        {
            SelectExpression select = (SelectExpression)Visit(scalar.Select);
            if (select != scalar.Select)
                return new ScalarExpression(scalar.Type, select);
            return scalar;
        }

        protected virtual Expression VisitSelect(SelectExpression select)
        {
            var from = VisitSource(select.From);
            var where = Visit(select.Where);
            var groupBy = Visit(select.GroupBy);
            var orderBy = VisitOrderBy(select.OrderBy);
            var skip = Visit(select.Skip);
            var take = Visit(select.Take);
            var fields = VisitFieldDeclarationList(select.Fields);
            if (from != select.From || where != select.Where || orderBy != select.OrderBy || groupBy != select.GroupBy || skip != select.Skip || take != select.Take || fields != select.Fields)
                return new SelectExpression(select.Alias, fields, from, where, orderBy, groupBy, select.IsDistinct, skip, take);
            return select;
        }

        protected virtual Expression VisitSource(Expression source)
        {
            return Visit(source);
        }

        protected virtual Expression VisitSubquery(SubqueryExpression subquery)
        {
            switch ((MongoExpressionType)subquery.NodeType)
            {
                case MongoExpressionType.Scalar:
                    return VisitScalar((ScalarExpression)subquery);
            }
            return subquery;
        }

        protected virtual ReadOnlyCollection<FieldDeclaration> VisitFieldDeclarationList(ReadOnlyCollection<FieldDeclaration> fields)
        {
            if (fields == null)
                return fields;

            List<FieldDeclaration> alternate = null;
            for (int i = 0, n = fields.Count; i < n; i++)
            {
                var f = fields[i];
                var e = Visit(f.Expression);
                if (f.Expression != e && alternate == null)
                    alternate = fields.Take(i).ToList();
                if (alternate != null)
                    alternate.Add(new FieldDeclaration(f.Name, e));
            }
            if (alternate != null)
                return alternate.AsReadOnly();
            return fields;
        }
    }
}