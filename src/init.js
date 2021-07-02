import {debounce} from 'debounce'
import {
  tap,
  __,
  addIndex,
  always,
  applyTo,
  append,
  assoc,
  both,
  complement,
  compose,
  cond,
  converge,
  curry,
  curryN,
  dissoc,
  either,
  equals,
  evolve,
  has,
  head,
  identity,
  ifElse,
  includes,
  last,
  lensPath,
  lensProp,
  map,
  mergeRight,
  not,
  objOf,
  of,
  over,
  path,
  pipe,
  prop,
  propEq,
  propOr,
  reduce,
  reject,
  sortBy,
  T,
  toPairs,
  tryCatch,
  unary,
  uniqBy,
  unless,
  when,
} from 'ramda'
import unitMath from 'unitmath'

import operators from './operators.json'
import units from './units.json'

// const log = (label) => (xs) => {
//   console.log(label)
//   console.log(xs)
//   return xs
// }

const initTreeOfFilterables = (filters = [], config = {}) => {
  /* kind of tricky */
  // const setAndDebounce = curry((config, action) =>
  //   ifElse(
  //     has('setter'),
  //     always(
  //       curryN(
  //         prop('length', action),
  //         debounce(
  //           pipe(action, prop('setter', config)),
  //           parseInt(propOr(50, 'debounce', config)),
  //         ),
  //       ),
  //     ),
  //     always(curry(action)),
  //   )(config),
  // )
  // const setAndDebounceWConfig = setAndDebounce(config)

  const sanitizeValueAccordingToOperator = curry((newOperator, obj) =>
    cond([
      [
        //if new and old operators are same, do nothing
        pipe(prop('operator'), equals(newOperator)),
        identity,
      ],
      [
        //if previous operator was between, reset value to start of range (int)
        pipe(prop('operator'), equals('between')),
        converge(assoc('value'), [pipe(prop('range'), head), identity]),
      ],
      [
        //if new operator is between, reset value to range ([])
        always(equals(newOperator, 'between')),
        converge(assoc('value'), [prop('range'), identity]),
      ],
      [
        //otherwise do nothing
        T,
        identity,
      ],
    ])(obj),
  )

  const getValueInNewUnit = curry((oldUnit, newUnit, oldValue) =>
    tryCatch(
      (val) => unitMath(val + oldUnit).to(newUnit).value,
      always(undefined),
    )(oldValue),
  )
  const assocRangeAccordingToUnit = curry((newUnit, obj) => {
    const newRange = pipe(
      map(getValueInNewUnit(prop('initialUnit', obj), newUnit)),
    )(prop('initialRange', obj))
    return assoc('range', newRange, obj)
  })
  const getApplicableUnits = (unit) =>
    tryCatch(
      pipe(
        (unit) => unitMath(unit).getQuantities(),
        head,
        prop(__, units),
        map(prop('name')),
      ),
      always([undefined]),
    )(unit)
  const getApplicablePrefixes = (unit) =>
    tryCatch(
      pipe(
        unitMath,
        path(['units', 0, 'unit', 'prefixes']),
        toPairs,
        sortBy(prop(1)),
        map(prop(0)),
      ),
      pipe(always([undefined])),
    )(unit)
  const getCurrentUnprefixedUnit = (unit) =>
    tryCatch(
      pipe(unitMath, path(['units', 0, 'unit', 'name'])),
      always(undefined),
    )(unit)
  const getCurrentPrefix = (unit) =>
    tryCatch(
      pipe(unitMath, path(['units', 0, 'prefix'])),
      always(undefined),
    )(unit)
  const initUnitDependentKeys = curry((unit, obj) =>
    pipe(
      //this
      assoc('applicableUnits', getApplicableUnits(unit)),
      assoc('applicablePrefixes', getApplicablePrefixes(unit)),
      assoc('currentUnprefixedUnit', getCurrentUnprefixedUnit(unit)),
      assoc('currentUnitPrefix', getCurrentPrefix(unit)),
    )(obj),
  )
  const updateUnitDependentKeys = curry((unit, obj) =>
    pipe(
      // ...
      assoc('applicablePrefixes', getApplicablePrefixes(unit)),
      assoc('currentUnprefixedUnit', getCurrentUnprefixedUnit(unit)),
      assoc('currentUnitPrefix', getCurrentPrefix(unit)),
      assocRangeAccordingToUnit(unit),
      //refactor
      ifElse(
        pipe(prop('operator', equals('between'))),
        converge(assoc('value'), [prop('range'), identity]),
        converge(assoc('value'), [pipe(prop('range'), head), identity]),
      ),
    )(obj),
  )

  const assign = (lens, prop, e) => {
    return over(
      lens,
      pipe(
        cond([
          [
            always(equals('operator', prop)),
            sanitizeValueAccordingToOperator(e),
          ],
          [always(equals('unit', prop)), updateUnitDependentKeys(e)],
          [T, identity],
        ]),
        assoc(prop, e),
      ),
    )
  }

  const toggleIsActive = curry((lens, e) => {
    return cond([
      [always(equals(1, e)), over(lens, assoc('isActive', true))],
      [always(equals(false, !!e)), over(lens, assoc('isActive', false))],
      [T, over(lens, evolve({isActive: not}))],
    ])
  })

  const toggleAndOr = (lens, e) =>
    cond([
      [always(equals(e, 'and')), over(lens, assoc('operator', 'and'))],
      [always(equals(e, 'or')), over(lens, assoc('operator', 'or'))],
      [
        T,
        over(
          lens,
          evolve({
            operator: ifElse(equals('and'), always('or'), always('and')),
          }),
        ),
      ],
    ])

  const toggleValueInList = (lens, e) => {
    const toggleValue = (e) => (list) =>
      ifElse(includes(e), reject(equals(e)), append(e))(list)
    return over(lens, evolve({value: toggleValue(e)}))
  }

  const filterLens = (itemIdx) => lensPath(['filters', itemIdx])
  const targetLens = (target) => lensProp(target)

  //this all needs to go into refactor machine, megaugly
  const setter = config?.setter
  const wrap = (action, set) =>
    set
      ? curryN(
          prop('length', action),
          debounce(
            pipe(action, set),
            parseInt(propOr(250, 'debounce', config)),
          ),
        )
      : curry(action)
  const toggleIsActiveW = wrap(toggleIsActive, setter)
  const toggleAndOrW = wrap(toggleAndOr, setter)
  const toggleValueInListW = wrap(toggleValueInList, setter)
  const assignW = wrap(assign, setter)

  return pipe(
    //enjoy the pointless ride
    uniqBy(prop('target')),
    map(
      pipe(
        //assoc targetLens to target
        converge(assoc('lens'), [
          pipe(propOr(of('root'), 'target'), last, targetLens),
          identity,
        ]),
        //assoc targetLens to target's filters
        converge(
          (targetLens, target) =>
            over(lensProp('filters'), map(assoc('lens', targetLens)), target),
          [prop('lens'), identity],
        ),
        unless(has('operator'), assoc('operator', 'and')),
        converge(assoc('toggleOperator'), [
          pipe(prop('lens'), toggleAndOrW),
          identity,
        ]),
        //do stuff to filters inside targets
        over(
          lensProp('filters'),
          addIndex(map)((item, idx) =>
            pipe(
              //evolve targetLens of each filter to filterLens
              evolve({lens: (lens) => unary(compose(lens, filterLens(idx)))}),
              assoc('isActive', false),
              converge(assoc('toggleIsActive'), [
                pipe(prop('lens'), toggleIsActiveW),
                identity,
              ]),
              when(
                both(has('name'), complement(has)('list')),
                converge(assoc('assocValue'), [
                  pipe(prop('lens'), assignW(__, 'value')),
                  identity,
                ]),
              ),
              when(
                has('initialUnit'),
                pipe(
                  converge(initUnitDependentKeys, [
                    prop('initialUnit'),
                    identity,
                  ]),
                  converge(assoc('assocUnit'), [
                    pipe(prop('lens'), assignW(__, 'unit')),
                    identity,
                  ]),
                  converge(assoc('unit'), [prop('initialUnit'), identity]),
                ),
              ),
              cond([
                //do things depending on kind of filterable
                [
                  either(has('initialRange'), propEq('kind', 'numeric')),
                  pipe(
                    converge(assoc('range'), [prop('initialRange'), identity]),
                    converge(assoc('value'), [prop('initialRange'), identity]),
                    unless(has('initialUnit'), dissoc('initialRange')),
                    unless(
                      has('applicableOperators'),
                      assoc('applicableOperators', operators.numeric),
                    ),
                  ),
                ],
                [
                  either(has('list'), propEq('kind', 'many')),
                  pipe(
                    unless(
                      has('applicableOperators'),
                      assoc('applicableOperators', operators.many),
                    ),
                    assoc('value', []),
                    converge(assoc('toggleValueInList'), [
                      pipe(prop('lens'), toggleValueInListW),
                      identity,
                    ]),
                  ),
                ],
                [
                  either(has('options'), propEq('kind', 'single')),
                  converge(assoc('value'), [
                    pipe(prop('options'), head),
                    identity,
                  ]),
                ],
                [
                  either(complement(has)('name'), propEq('kind', 'bool')),
                  identity,
                ],
                [
                  T,
                  pipe(
                    unless(
                      has('applicableOperators'),
                      assoc('applicableOperators', operators.text),
                    ),
                    assoc('value', ''),
                  ),
                ],
              ]),
              when(
                //do things to all fiterables that have operators
                has('applicableOperators'),
                pipe(
                  unless(
                    has('operator'),
                    converge(assoc('operator'), [
                      pipe(prop('applicableOperators'), head),
                      identity,
                    ]),
                  ),
                  converge(assoc('assocOperator'), [
                    pipe(prop('lens'), assignW(__, 'operator')),
                    identity,
                  ]),
                ),
              ),
            )(item),
          ),
        ),
        //create target object key value pair with deepest target being the key, if no target then key is root
        converge(objOf, [
          ifElse(has('target'), pipe(prop('target'), last), always('root')),
          identity,
        ]),
      ),
    ),
    reduce(mergeRight, {}),
  )(filters)
}

export default initTreeOfFilterables
