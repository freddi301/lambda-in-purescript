module Lambda.Control.Infere where

-- // @flow

-- import { Ast, Abs, App, Var } from '../ast';
-- import { Map, List } from 'immutable';

-- export class InvalidAstError extends Error {
--   ast: Ast;
--   constructor(ast: Ast) {
--     super(`invalid ast`);
--     this.ast = ast;
--   }
-- }

-- export interface TypeScope {
--   get(i: Var): number;
--   set(i: Var, t: number): TypeScope
-- }

-- export class TypeAstNotFoundInScopeError extends Error {
--   identifier: Var;
--   scope: TypeScope;
--   constructor(i: Var, scope: TypeScope) {
--     super(`identifier: ${String(i)} not found in type scope: ${String(scope)}`);
--     this.identifier = i;
--     this.scope = scope;
--   }
-- }

-- export class StringTypeScope {
--   dict: { [id: string]: number };
--   constructor(dict: { [id: string]: number } = {}) { this.dict = dict; }
--   get(i: Var) { const got = this.dict[i.name]; if (!got) throw new TypeAstNotFoundInScopeError(i, this); return got; }
--   set(i: Var, l: number) { return new StringTypeScope({ ...this.dict, [i.name]: l }); }
--   toString() { return JSON.stringify(this.dict); }
-- }

-- export class Constraint<T> {
--   ast: Ast;
--   constructor(ast: Ast) { this.ast = ast; }
--   toStringRec(map: Map<T, List<Constraint<T>>>) { map; throw new Error("must be implemented"); }
-- } // eslint-disable-line no-unused-vars
-- export class IsAbs<T> extends Constraint<T> {
--   head: T; body: T;
--   constructor(head: T, body: T, ast: Ast) { super(ast); this.head = head; this.body = body; }
--   toString() { return `${String(this.head)} -> ${String(this.body)}`; }
--   toStringRec(map: Map<T, List<Constraint<T>>>) {
--     const head = map.get(this.head) ? map.get(this.head).map(c => c.toStringRec(map)).toJS().join('<>') : String(this.head);
--     const body = map.get(this.body) ? map.get(this.body).map(c => c.toStringRec(map)).toJS().join('<>') : String(this.body);
--     return `(${head} -> ${body})`;
--   }
-- }

-- export class Constraints<Type> {
--   typeToConstraintsMap: Map<Type, List<Constraint<Type>>>;
--   constructor(typeToConstraintsMap: Map<Type, List<Constraint<Type>>> = Map()) { this.typeToConstraintsMap = typeToConstraintsMap; }
--   add(type: Type, constraint: Constraint<Type>): Constraints<Type> {
--     return new Constraints(this.typeToConstraintsMap.updateIn([type], List(), l => l.push(constraint)));
--   }
--   findConstraint(type: Type, constraint: Class<Constraint<Type>>): Constraint<Type>[] {
--     return this.typeToConstraintsMap.get(type, List()).filter(c => c instanceof constraint).toJS();
--   }
--   // TODO: replace() replace all occurences of type
--   matchInferredAbs(): Constraints<Type> {
--     // TODO: if IsAbs already in constraints list for a type,
--     // replace head and body occurences
--     // repeat until nothing changes
--     return this;
--   }
--   toJSON() { return this.typeToConstraintsMap.toJS(); }
--   toString() {
--     return Object.entries(this.typeToConstraintsMap.toJS())
--     .reduce((memo, [key, value]) => {
--       const constraints = (value: any).map(String).join('; ');
--       const expandedConstraints = (value: any).map(c => c.toStringRec(this.typeToConstraintsMap)).join('; ');
--       const asts = (value: any).map(c => String(c.ast)).join('~ ');
--       memo.push(`${asts} :: ${key} ${constraints} | ${expandedConstraints}`);
--       return memo;
--     }, [])
--     .join('\n');
--   }
--   stringifyType(type: Type, extra: 'AbsNoParens' | '' = ''): string {
--     return this.typeToConstraintsMap.get(type, List([type])).toJS().map(c => {
--       if (c instanceof IsAbs) {
--         const pl = extra === 'AbsNoParens' ? '' : '(';
--         const pr = extra === 'AbsNoParens' ? '' : ')';
--         if (this.findConstraint(c.body, IsAbs).length) { // right associative
--           return `${pl}${this.stringifyType(c.head)}->${this.stringifyType(c.body, 'AbsNoParens')}${pr}`;
--         }
--         return `${pl}${this.stringifyType(c.head)}->${this.stringifyType(c.body)}${pr}`;
--       }
--       return String(type);
--     }).join('\n');
--   }
-- }

-- export function infere(
--   { ast, nextType, typeScope, constraints }: { ast: Ast, nextType: number, typeScope: TypeScope, constraints: Constraints<number> }
-- ): { type: number, nextType: number, constraints: Constraints<number> } {
--   if (ast instanceof App) {
--     const left = ast.left instanceof Abs ? ast.left : null;
--     const right = ast.right instanceof Abs ? ast.right : null;
--     if (left && right) {
--       const inferred = infere({ ast: right, nextType, typeScope, constraints });
--       return infere({
--         ast: left.body, nextType: inferred.nextType,
--         typeScope: typeScope.set(left.head, inferred.type), constraints: inferred.constraints,
--       });
--     } else { // eslint-disable-line no-else-return
--       const inferredRigth = infere({ ast: ast.right, nextType, typeScope, constraints });
--       const rt = inferredRigth.nextType;
--       const inferredLeft = infere({
--         ast: ast.left, nextType: inferredRigth.nextType + 1,
--         typeScope, constraints: inferredRigth.constraints,
--       });
--       const newConstraints = !constraints.findConstraint(inferredLeft.type, IsAbs).length ? // ensure is only typeholder
--         inferredLeft.constraints.add(inferredLeft.type, new IsAbs(inferredRigth.type, rt, ast)) : inferredLeft.constraints;
--       return { type: rt, nextType: inferredLeft.nextType, constraints: newConstraints };
--     }
--   } else if (ast instanceof Var) {
--     return { type: typeScope.get(ast), nextType, typeScope, constraints };
--   } else if (ast instanceof Abs) {
--     const thisAbsType = nextType;
--     const thisAbsHeadType = nextType + 1;
--     const inferred = infere({ ast: ast.body, nextType: nextType + 2, typeScope: typeScope.set(ast.head, thisAbsHeadType), constraints });
--     const newConstraints = inferred.constraints.add(thisAbsType, new IsAbs(thisAbsHeadType, inferred.type, ast));
--     return { type: nextType, nextType: inferred.nextType, constraints: newConstraints };
--   }
--   throw new InvalidAstError(ast);
-- }

-- export const inf = (ast: Ast) => infere({
--     ast, nextType: 1, typeScope: new StringTypeScope(), constraints: new Constraints()
--   });

-- // TODO: add typechecking (trace)

-- // TODO: add user definable constraints

-- // TODO: add to JSON serialization

