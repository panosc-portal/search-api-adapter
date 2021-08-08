import {
  __,
  add,
  addIndex,
  always,
  assoc,
  assocPath,
  both,
  concat,
  cond,
  converge,
  descend,
  dissoc,
  either,
  equals,
  filter,
  flip,
  gt,
  has,
  head,
  identity,
  ifElse,
  is,
  isEmpty,
  isNil,
  juxt,
  last,
  length,
  lensPath,
  lensProp,
  map,
  mergeDeepRight,
  multiply,
  objOf,
  over,
  pick,
  pipe,
  prop,
  propOr,
  reduce,
  reduceRight,
  reject,
  slice,
  sort,
  subtract,
  T,
  uniq,
  unless,
  unnest,
  useWith,
  values,
  when,
} from 'ramda'

// const log = (label) => (xs) => {
//   console.log(label)
//   console.log(xs)
//   return xs
// }

const enhancePagination = (config) =>
  pipe(
    unless(
      either(has('limit'), has('pageSize'), has('page')),
      assoc('limit', 25),
    ),
    when(
      either(has('pageSize'), has('page')),
      pipe(
        converge(assoc('limit'), [
          propOr(25, 'pageSize'),
          identity,
        ]),
        converge(assoc('skip'), [
          converge(multiply, [
            propOr(25, 'pageSize'),
            pipe(propOr(1, 'page'), subtract(__, 1)),
          ]),
          identity,
        ]),
      ),
    ),
    when(pipe(prop('limit'), equals(false)), dissoc('limit')),
    dissoc('page'),
    dissoc('pageSize'),
  )(config)

const filterInactiveAndParseToArray = (filterables) =>
  pipe(
    map(
      pipe(
        over(lensProp('filters'), pipe(values, filter(prop('isActive')))),
        cond([
          [pipe(prop('filters'), length, gt(1)), always({})],
          [
            pipe(prop('filters'), length, gt(2)),
            ifElse(
              has('target'),
              converge(assoc('target'), [
                prop('target'),
                pipe(prop('filters'), head),
              ]),
              pipe(prop('filters'), head),
            ),
          ],
          [T, unless(has('operator'), assoc('operator', 'and'))],
        ]),
      ),
    ),
    values,
    reject(isEmpty),
  )(filterables)

const makePathFromTarget = reduceRight(
  (val, acc) =>
    isEmpty(acc)
      ? {include: {[val]: {relation: val}}}
      : {include: {[val]: {relation: val, scope: acc}}},
  {},
)
const getPathFromTarget = addIndex(reduceRight)(
  (val, acc, idx, list) =>
    idx === list.length - 1
      ? ['include', val, ...acc]
      : ['scope', 'include', val, ...acc],
  ['scope', 'where'],
)

const makeNonParameter = converge(objOf, [
  propOr('name', 'name'),
  ifElse(
    has('operator'),
    converge(objOf, [prop('operator'), prop('value')]),
    prop('value'),
  ),
])

const makeParameter = pipe(
  juxt([
    pick(['name']),
    pipe(
      ifElse(
        has('operator'),
        converge(objOf, [prop('operator'), prop('value')]),
        prop(['value']),
      ),
      objOf('value'),
    ),
    pipe(prop('unit'), objOf('unit')),
  ]),
  reject(both(has('unit'), pipe(prop('unit'), isNil))),
  objOf('and'),
)

const isParameter = pipe(propOr([], 'target'), last, equals('parameters'))

const makeObjectFromConfig = when(
  has('include'),
  over(
    lensProp('include'),
    pipe(
      map(pipe(makePathFromTarget, prop('include'))),
      reduce(mergeDeepRight, {}),
    ),
  ),
)

const makeFilter = ifElse(
  isParameter,
  ifElse(
    has('filters'),
    converge(objOf, [
      prop('operator'),
      pipe(prop('filters'), map(makeParameter)),
    ]),
    makeParameter,
  ),
  ifElse(
    has('filters'),
    converge(objOf, [
      prop('operator'),
      pipe(prop('filters'), map(makeNonParameter)),
    ]),
    makeNonParameter,
  ),
)

const makeObjectFromFilters = pipe(
  map(
    ifElse(
      has('target'),
      converge(assocPath, [
        pipe(prop('target'), getPathFromTarget),
        makeFilter,
        pipe(prop('target'), makePathFromTarget),
      ]),
      pipe(makeFilter, objOf('where')),
    ),
  ),
  reduce(mergeDeepRight, {}),
)

const getPathsFromConfig = pipe(propOr([], 'include'), map(getPathFromTarget))

const getPathsFromFilters = pipe(
  filter(has('target')),
  map(pipe(prop('target'), getPathFromTarget)),
)

const makeUniqueSortedPathsToIncludeKeys = pipe(
  concat,
  map(
    pipe(
      addIndex(map)((item, idx, list) => {
        //fml
        return when(
          equals('include'),
          always(slice(0, add(1, idx), list)),
        )(item)
      }),
      filter(is(Array)),
    ),
  ),
  unnest,
  uniq,
  sort(descend(length)),
)

const getPathsDueCleanup = useWith(makeUniqueSortedPathsToIncludeKeys, [
  getPathsFromConfig,
  getPathsFromFilters,
])

//this looks more complicated than it should
const cleanup = flip(useWith(over(__, values, __), [lensPath, identity]))

const makeQuery = converge(
  pipe(reduce(cleanup), enhancePagination, JSON.stringify, encodeURIComponent),
  [
    useWith(mergeDeepRight, [makeObjectFromConfig, makeObjectFromFilters]),
    getPathsDueCleanup,
  ],
)

const generator = (filters, config) =>
  makeQuery(config, filterInactiveAndParseToArray(filters))

export default generator
