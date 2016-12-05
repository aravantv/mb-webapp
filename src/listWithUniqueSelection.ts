import { Stream } from 'xstream'
import { div, ul, input, VNode } from '@cycle/dom'
import { DOMSource } from '@cycle/dom/xstream-typings'
import { isEnter, getText } from './lib'
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

export function ListWithUniqueSelection(ri: RawInput): Output {
  const keyups$ = ri.dom.select('.field2').events('keyup')
  const acceptNew$ = keyups$.filter(isEnter).map(getText)
  const add$ = acceptNew$.map(initialValue => {
    return { initialValue }
  })

  const list$ = Collection(ri2 =>
    SelectableText(keyMousePreprocessor(ri2)
      .startWith({ type: ItemInput.Confirm })
      .startWith({ type: ItemInput.Change, payload: ri2.initialValue }))
    , ri, add$)

  let itemVtrees$ = Collection.pluck(list$, (item: ItemOutput) => item.dom)
  const field = input('.field2', { props: { type: 'text', value: get_hack_last_value() } })
  const dom = itemVtrees$.map(addedComps =>
    div([
      field,

      ul(addedComps)
    ])
  return {
    dom,
  }
}
