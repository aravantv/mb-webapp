import { isEnter, isEscape, getText, extend, Input, filterOnType } from './lib'
import { Stream } from 'xstream'
import { span, input, VNode } from '@cycle/dom'
import { DOMSource } from '@cycle/dom/xstream-typings'

export interface RawInput {
  dom: DOMSource
}

export interface Output {
  dom: Stream<VNode>
}

export const enum InputType {
  Confirm, // no payload
  Cancel, // no payload
  Select, // no payload
  Change, // payload: string
}

interface IState {
  uiValue: string,
  validatedName: string,
  isSelected: boolean,
}

function getChange(ev: Event) {
  return { type: InputType.Change, payload: getText(ev) }
}

// "Intent"
export function keyMousePreprocessor(i: RawInput): Input<InputType> {
  const keyups$ = i.dom.select('.field').events('keyup')
  return Stream.merge(
    keyups$.filter(isEnter).mapTo({ type: InputType.Confirm }),
    keyups$.filter(isEscape).mapTo({ type: InputType.Cancel }),
    keyups$.filter(ev => !isEscape(ev) && !isEnter(ev)).map(getChange),
    i.dom.select('.field').events('dblclick').mapTo({ type: InputType.Select })
  )
}

// "Model"
function react$(i: Input<InputType>, initState: IState): Stream<IState> {
  return Stream.merge(
    filterOnType(i, InputType.Change).map(n => s =>
      extend({ uiValue: n.payload }, s)),
    filterOnType(i, InputType.Confirm).mapTo(s =>
      extend({ validatedName: s.uiValue, isSelected: false }, s)),
    filterOnType(i, InputType.Cancel).mapTo(s => extend({ uiValue: s.validatedName, isSelected: false }, s)),
    filterOnType(i, InputType.Select).mapTo(s => extend({ isSelected: true }, s))
  ).fold((acc, f: (prev: IState) => IState) => f(acc), initState)
}

// "View"
function render$(s$: Stream<IState>): Stream<VNode> {
  return s$.map(s =>
    s.isSelected
      ? input('.field', { props: { type: 'text', value: s.uiValue } })
      : span('.field', s.validatedName))
}

export function SelectableText(i: Input<InputType>): Output {
  const state$ = react$(i.debug(), { isSelected: true, uiValue: '', validatedName: null })
  return { dom: render$(state$) }
}
