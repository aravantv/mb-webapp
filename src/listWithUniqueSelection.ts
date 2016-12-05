import { Stream } from 'xstream'
import { li, ul, input, VNode } from '@cycle/dom'
import { DOMSource } from '@cycle/dom/xstream-typings'
import { isEnter, getText, extend } from './lib'
import { SelectableText, keyMousePreprocessor, InputType as ItemInput, Output as ItemOutput } from './selectableText'
import Collection from '@cycle/collection'

export interface RawInput {
  dom: DOMSource
}

export interface Output {
  dom: Stream<VNode>
}

interface Input {
  added$: Stream<string>,
  deleted$: Stream<number>,
}

type VNodeStream = Stream<VNode>

/** Hack to force the input field to refresh. */
let hack_last_value = ''
function get_hack_last_value() {
  hack_last_value = hack_last_value === '' ? null : ''
  return hack_last_value
}

function Item(ri: RawInput & { initialValue: string }) {
  const v = SelectableText(
    keyMousePreprocessor(ri)
      .startWith({ type: ItemInput.Confirm })
      .startWith({ type: ItemInput.Change, payload: ri.initialValue })
  )
  return extend({ dom: v.dom.map(vtree => li([vtree])) }, v)
}

export function ListWithUniqueSelection(ri: RawInput): Output {
  const keyups$ = ri.dom.select('.field2').events('keyup')
  const acceptNew$ = keyups$.filter(isEnter).map(getText)
  const add$ = acceptNew$.map(initialValue => {
    return { initialValue }
  })

  const list$ = Collection(Item, ri, add$)

  let itemVtrees$ = Collection.pluck(list$, (item: ItemOutput) => item.dom)
  const field = li([input('.field2', { props: { type: 'text', value: get_hack_last_value() } })])
  const dom = itemVtrees$.map(([...addedComps]) => ul([...addedComps, field].reverse()))
  return {
    dom,
  }
}
