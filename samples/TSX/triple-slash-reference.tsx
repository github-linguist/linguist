/// <reference path="../DefinitelyTyped/react/react-global.d.ts" />

// Fixture taken from https://github.com/RyanCavanaugh/koany/blob/master/koany.tsx

interface Garden {
	colors: Gardens.RockColor[];
	shapes: Gardens.RockShape[];
}

namespace Gardens {
	export enum RockShape {
		Empty,
		Circle,
		Triangle,
		Square,
		Max
	}
	export const RockShapes = [RockShape.Circle, RockShape.Triangle, RockShape.Square];
	export const RockShapesAndEmpty = RockShapes.concat(RockShape.Empty);

	export enum RockColor {
		Empty,
		White,
		Red,
		Black,
		Max
	}
	export const RockColors = [RockColor.White, RockColor.Red, RockColor.Black];
	export const RockColorsAndEmpty = RockColors.concat(RockColor.Empty);

	export const Size = 9;

	// 012
	// 345
	// 678
	export const adjacencies = [
		[1, 3], [0, 4, 2], [1, 5],
		[0, 4, 6], [3, 1, 7, 5], [2, 4, 8],
		[3, 7], [6, 4, 8], [7, 5]
	];
}

module Koan {
	export enum DescribeContext {
		// every "white stone" is ...
		Singular,
		// all "white stones" are
		Plural,
		// every stone in the top row is "white"
		Adjectival
	}

	export enum PartType {
		Selector,
		Aspect
	}

	export enum StateTestResult {
		Fail = 0,
		WeakPass = 1,
		Pass = 2
	}

	/// A general format for producing a Statement
	export interface StatementTemplate<T> {
		holes: PartType[];
		describe(args: T): string;
		test(g: Garden, args: T): StateTestResult;
	}

	/// A completed rule that can be used to test a Garden
	export interface ProducedStatement<T> {
		test(g: Garden): StateTestResult;
		description: string;
		children: T;

		hasPassedAndFailed(): boolean;
	}

	function rnd(max: number) {
		return Math.floor(Math.random() * max);
	}

	function randomColor(): Gardens.RockColor {
		return Math.floor(Math.random() * (Gardens.RockColor.Max - 1)) + 1
	}

	function randomShape(): Gardens.RockShape {
		return Math.floor(Math.random() * (Gardens.RockShape.Max - 1)) + 1
	}

	/* New Impl Here */
	interface SelectorSpec<T> {
		childTypes?: PartType[];
		precedence: number;
		weight: number;
		test(args: T, g: Garden, index: number): string|number|boolean;
		describe(args: T, context: DescribeContext): string;
		isAllValues(values: Array<string>|Array<number>): boolean;
	}

	interface ProducedSelector {
		test(g: Garden, index: number): string|number|boolean;
		getDescription(plural: DescribeContext): string;
		seenAllValues(): boolean;
	}

	export function buildSelector<T>(spec: SelectorSpec<T>, args: T): ProducedSelector {
		let seenResults: { [s: string]: boolean;} = {};
		return {
			test: (g: Garden, index: number) => {
				var result = spec.test(args, g, index);
				seenResults[result + ''] = true;
				return result;
			},
			getDescription: (context) => {
				return spec.describe(args, context);
			},
			seenAllValues: () => {
				return spec.isAllValues(Object.keys(seenResults));
			}
		}
	}

	export var SelectorTemplates: Array<SelectorSpec<{}>> = [];
	module LetsMakeSomeSelectors {
		// Is rock
		SelectorTemplates.push({
			test: (args, g, i) => g.colors[i] !== Gardens.RockColor.Empty,
			describe: (args, context) => {
				switch(context) {
					case DescribeContext.Plural:
						return 'Stones';
					case DescribeContext.Adjectival:
						return 'not empty';
					case DescribeContext.Singular:
						return 'Stone';
				}
			},
			isAllValues: items => items.length === 2,
			precedence: 0,
			weight: 1
		});

		// Is of a certain color and/or shape
		Gardens.RockColorsAndEmpty.forEach(color => {
			let colorName = Gardens.RockColor[color];
			let colorWeight = color === Gardens.RockColor.Empty ? 1 : 0.33;
			Gardens.RockShapesAndEmpty.forEach(shape => {
				let shapeName = Gardens.RockShape[shape];
				let shapeWeight = shape === Gardens.RockShape.Empty ? 1 : 0.33;
				SelectorTemplates.push({
					test: (args, g, i) => {
						if(color === Gardens.RockColor.Empty) {
							if (shape === Gardens.RockShape.Empty) {
								return g.colors[i] === Gardens.RockColor.Empty;
							} else {
								return g.shapes[i] === shape;
							}
						} else {
							if (shape === Gardens.RockShape.Empty) {
								return g.colors[i] === color;
							} else {
								return g.shapes[i] === shape && g.colors[i] === color;
							}
						}
					},
					describe: (args, context) => {
						if(color === Gardens.RockColor.Empty) {
							if (shape === Gardens.RockShape.Empty) {
								switch(context) {
									case DescribeContext.Plural:
										return 'Empty Cells';
									case DescribeContext.Adjectival:
										return 'Empty';
									case DescribeContext.Singular:
										return 'Empty Cell';
								}
							} else {
								switch(context) {
									case DescribeContext.Plural:
										return shapeName + 's';
									case DescribeContext.Adjectival:
										return 'a ' + shapeName;
									case DescribeContext.Singular:
										return shapeName;
								}
							}
						} else {
							if (shape === Gardens.RockShape.Empty) {
								switch(context) {
									case DescribeContext.Plural:
										return colorName + ' Stones';
									case DescribeContext.Adjectival:
										return colorName;
									case DescribeContext.Singular:
										return colorName + ' Stone';
								}
							} else {
								switch(context) {
									case DescribeContext.Plural:
										return colorName + ' ' + shapeName + 's';
									case DescribeContext.Adjectival:
										return 'a ' + colorName + ' ' + shapeName;
									case DescribeContext.Singular:
										return colorName + ' ' + shapeName;
								}
							}
						}
					},
					isAllValues: items => items.length === 2,
					precedence: 3,
					weight: (shapeWeight + colorWeight === 2) ? 0.3 : shapeWeight * colorWeight
				});
			});
		});

		// [?] in the [top|middle|bottom] [row|column]
		[0, 1, 2].forEach(rowCol => {
			[true, false].forEach(isRow => {
				var name = (isRow ? ['top', 'middle', 'bottom'] : ['left', 'middle', 'right'])[rowCol] + ' ' + (isRow ? 'row' : 'column');
				var spec: SelectorSpec<[ProducedSelector]> = {
					childTypes: [PartType.Selector],
					test: (args, g, i) => {
						var c = isRow ? Math.floor(i / 3) : i % 3;
						if (c === rowCol) {
							return args[0].test(g, i);
						} else {
							return false;
						}
					},
					describe: (args, plural) => args[0].getDescription(plural) + ' in the ' + name,
					isAllValues: items => items.length === 2,
					precedence: 4,
					weight: 1 / 6
				};
				SelectorTemplates.push(spec);
			});
		});

		// [?] next to a [?]
		SelectorTemplates.push({
			childTypes: [PartType.Selector, PartType.Selector],
			test: (args, g, i) => {
				if (args[0].test(g, i)) {
					return Gardens.adjacencies[i].some(x => !!args[1].test(g, x));
				} else {
					return false;
				}
			},
			describe: (args, plural) => {
				return args[0].getDescription(plural) + ' next to a ' + args[1].getDescription(DescribeContext.Singular);
			},
			isAllValues: items => items.length === 2,
			precedence: 4,
			weight: 1
		} as SelectorSpec<[ProducedSelector, ProducedSelector]>);
	}

	export function buildStatement<T>(s: StatementTemplate<T>, args: T): ProducedStatement<T> {
		let hasPassed = false;
		let hasFailed = false;

		let result: ProducedStatement<T> = {
			children: args,
			description: s.describe(args),
			test: (g) => {
				let r = s.test(g, args);
				if (r === StateTestResult.Pass) {
					hasPassed = true;
				} else if(r === StateTestResult.Fail) {
					hasFailed = true;
				}
				return r;
			},
			hasPassedAndFailed: () => {
				return hasPassed && hasFailed && (args as any as ProducedSelector[]).every(c => c.seenAllValues());
			}
		};
		return result;
	}

	export let StatementList: StatementTemplate<any>[] = [];
	module LetsMakeSomeStatements {
		// Every [?] is a [?]
		StatementList.push({
			holes: [PartType.Selector, PartType.Selector],
			test: (g: Garden, args: [ProducedSelector, ProducedSelector]) => {
				let didAnyTests = false;
				for (var i = 0; i < Gardens.Size; i++) {
					if (args[0].test(g, i)) {
						if(!args[1].test(g, i)) return StateTestResult.Fail;
						didAnyTests = true;
					}
				}
				return didAnyTests ? StateTestResult.Pass : StateTestResult.WeakPass;
			},
			describe: args => {
				return 'Every ' + args[0].getDescription(DescribeContext.Singular) + ' is ' + args[1].getDescription(DescribeContext.Adjectival);
			}
		});

		// There is exactly 1 [?]
		StatementList.push({
			holes: [PartType.Selector],
			test: (g: Garden, args: [ProducedSelector, ProducedSelector]) => {
				var count = 0;
				for (var i = 0; i < Gardens.Size; i++) {
					if (args[0].test(g, i)) count++;
				}

				return count === 1 ? StateTestResult.Pass : StateTestResult.Fail;
			},
			describe: args => {
				return 'There is exactly one ' + args[0].description;
			}
		});

		// There are more [?] than [?]
		StatementList.push({
			holes: [PartType.Selector, PartType.Selector],
			test: (g: Garden, args: [ProducedSelector, ProducedSelector]) => {
				var p1c = 0, p2c = 0;
				for (var i = 0; i < Gardens.Size; i++) {
					if (args[0].test(g, i)) p1c++;
					if (args[1].test(g, i)) p2c++;
				}
				if(p1c > p2c && p2c > 0) {
					return StateTestResult.Pass;
				} else if(p1c > p2c) {
					return StateTestResult.WeakPass;
				} else {
					return StateTestResult.Fail;
				}
			},
			describe: args => {
				return 'There are more ' + args[0].descriptionPlural + ' than ' + args[1].descriptionPlural;
			}
		});
	}

	function randomElementOf<T>(arr: T[]): T {
		if (arr.length === 0) {
			return undefined;
		} else {
			return arr[Math.floor(Math.random() * arr.length)];
		}
	}

	function randomWeightedElementOf<T extends { weight: number }>(arr: T[]): T {
		var totalWeight = arr.reduce((acc, v) => acc + v.weight, 0);
		var rnd = Math.random() * totalWeight;
		for (var i = 0; i < arr.length; i++) {
			rnd -= arr[i].weight;
			if (rnd <= 0) return arr[i];
		}
		// Got destroyed by floating error, just try again
		return randomWeightedElementOf(arr);
	}

	export function buildRandomNewSelector(maxPrecedence = 1000000): ProducedSelector {
		var choices = SelectorTemplates;

		let initial = randomWeightedElementOf(choices.filter(p => p.precedence <= maxPrecedence));
		// Fill in the holes
		if (initial.childTypes) {
			var fills = initial.childTypes.map(h => {
				if (h === PartType.Selector) {
					return buildRandomNewSelector(initial.precedence - 1);
				} else {
					throw new Error('Only know how to fill Selector holes')
				}
			});
			return buildSelector(initial, fills);
		} else {
			return buildSelector(initial, []);
		}
	}

	export function makeEmptyGarden(): Garden {
		var g = {} as Garden;
		g.colors = [];
		g.shapes = [];
		for (var i = 0; i < Gardens.Size; i++) {
			g.colors.push(Gardens.RockColor.Empty);
			g.shapes.push(Gardens.RockShape.Empty);
		}

		return g;
	}

	export function gardenToString(g: Garden): string {
		return g.colors.join('') + g.shapes.join('');
	}

	export function makeRandomGarden(): Garden {
		var g = makeEmptyGarden();
		blitRandomGardenPair(g, g);
		return g;
	}

	export function cloneGarden(g: Garden): Garden {
		var result: Garden = {
			colors: g.colors.slice(0),
			shapes: g.shapes.slice(0)
		};
		return result;
	}

	export function clearGarden(g: Garden) {
		for (var i = 0; i < Gardens.Size; i++) {
			g.colors[i] = Gardens.RockColor.Empty;
			g.shapes[i] = Gardens.RockShape.Empty;
		}
	}

	export function blitRandomGardenPair(g1: Garden, g2: Garden): void {
		let placeCount = 0;
		for (var i = 0; i < Gardens.Size; i++) {
			if (rnd(7) === 0) {
				g1.colors[i] = g2.colors[i] = randomColor();
				g1.shapes[i] = g2.shapes[i] = randomShape();
			} else {
				placeCount++;
				g1.colors[i] = g2.colors[i] = Gardens.RockColor.Empty;
				g1.shapes[i] = g2.shapes[i] = Gardens.RockShape.Empty;
			}
		}
		if (placeCount === 0) blitRandomGardenPair(g1, g2);
	}

	export function blitNumberedGarden(g: Garden, stoneCount: number, n: number): void {
		clearGarden(g);

		let cellNumbers = [0, 1, 2, 3, 4, 5, 6, 7, 8];
		for (let i = 0; i < stoneCount; i++) {
			let cellNum = getValue(cellNumbers.length);
			let cell = cellNumbers.splice(cellNum, 1)[0];
			g.colors[cell] = getValue(3) + 1;
			g.shapes[cell] = getValue(3) + 1;
		}

		function getValue(max: number) {
			let result = n % max;
			n = (n - result) / max;
			return result;
		}
	}

	export function mutateGarden(g: Garden): void {
		while (true) {
			var op = rnd(5);
			let x = rnd(Gardens.Size);
			let y = rnd(Gardens.Size);
			switch (op) {
				case 0: // Swap two non-identical cells
					if (g.colors[x] !== g.colors[y] || g.shapes[x] !== g.shapes[y]) {
						var tmp: any = g.colors[x];
						g.colors[x] = g.colors[y];
						g.colors[y] = tmp;
						tmp = g.shapes[x];
						g.shapes[x] = g.shapes[y];
						g.shapes[y] = tmp;
						return;
					}
					break;
				case 1: // Add a stone
					if (g.colors[x] === Gardens.RockColor.Empty) {
						g.colors[x] = randomColor();
						g.shapes[x] = randomShape();
						return;
					}
					break;
				case 2: // Remove a stone
					if (g.colors.filter(x => x !== Gardens.RockColor.Empty).length === 1) continue;

					if (g.colors[x] !== Gardens.RockColor.Empty) {
						g.colors[x] = Gardens.RockColor.Empty;
						g.shapes[x] = Gardens.RockShape.Empty;
						return;
					}
					break;
				case 3: // Change a color
					let c = randomColor();
					if (g.colors[x] !== Gardens.RockColor.Empty && g.colors[x] !== c) {
						g.colors[x] = c;
						return;
					}
					break;
				case 4: // Change a shape
					let s = randomShape();
					if (g.shapes[x] !== Gardens.RockShape.Empty && g.shapes[x] !== s) {
						g.shapes[x] = s;
						return;
					}
					break;
			}
		}
	}
}

class Indexion {
	sizes: number[];
	constructor(...sizes: number[]) {
		this.sizes = sizes;
	}

	public getValues(index: number): number[] {
		let result = new Array<number>(this.sizes.length);
		this.fillValues(index, result);
		return result;
	}

	public fillValues(index: number, result: number[]): void {
		for (var i = 0; i < this.sizes.length; i++) {
			result[i] = index % this.sizes[i];
			index -= result[i];
			index /= this.sizes[i];
		}
	}

	public valuesToIndex(values: number[]): number {
		var result = 0;
		var factor = 1;
		for (var i = 0; i < this.sizes.length; i++) {
			result += values[i] * this.sizes[i] * factor;
			factor *= this.sizes[i];
		}
		return result;
	}

	public getAdjacentIndices(index: number): number[][] {
		var baseline = this.getValues(index);
		var results: number[][] = [];
		for (var i = 0; i < this.sizes.length; i++) {
			if(baseline[i] > 0) {
				baseline[i]--;
				results.push(baseline.slice());
				baseline[i]++;
			}
			if(baseline[i] < this.sizes[i] - 1) {
				baseline[i]++;
				results.push(baseline.slice());
				baseline[i]--;
			}
		}
		return results;
	}

	public distance(index1: number, index2: number): number {
		let delta = 0;
		for (var i = 0; i < this.sizes.length; i++) {
			var a = index1 % this.sizes[i];
			var b = index2 % this.sizes[i];
			delta += Math.abs(b - a);
			index1 -= a;
			index2 -= b;
			index1 /= this.sizes[i];
			index2 /= this.sizes[i];
		}
		return delta;
	}
}


function makeNewExample() {
	while (true) {
		var p1 = Koan.buildSelector(Koan.SelectorTemplates[12], []);
		var p2 = Koan.buildSelector(Koan.SelectorTemplates[14], []);
		var test = Koan.buildStatement(Koan.StatementList[0], [p1, p2]);

		var examples: Garden[] = [];

		console.log('Attempt to generate examples for "' + test.description + '"');

		var maxGarden = /*(9 * 9) + (9 * 9 * 9 * 8) + */(9 * 9 * 9 * 8 * 9 * 7);
		let g = Koan.makeEmptyGarden();
		let passCount = 0, failCount = 0;
		let resultLookup: boolean[] = [];
		let lastResult: boolean = undefined;
		for (var i = 0; i < maxGarden; i++) {
			Koan.blitNumberedGarden(g, 3, i);
			let result = test.test(g);
			if(result === Koan.StateTestResult.Pass) {
				resultLookup[i] = true;
				passCount++;

				if (lastResult !== true && examples.length < 10) examples.push(Koan.cloneGarden(g));
				lastResult = true;
			} else if (result === Koan.StateTestResult.Fail) {
				resultLookup[i] = false;
				failCount++;

				if (lastResult !== false && examples.length < 10) examples.push(Koan.cloneGarden(g));
				lastResult = false;
			}

			if (examples.length === 10) break;
		}

		console.log('Rule passes ' + passCount + ' and fails ' + failCount);

		/*
		if (!test.hasPassedAndFailed()) {
			console.log('Rule has unreachable, contradictory, or tautological clauses');
			continue;
		}

		if (passCount === 0 || failCount === 0) {
			console.log('Rule is always true or always false');
			continue;
		}
		*/

		var h = document.createElement('h2');
		h.innerText = test.description;
		document.body.appendChild(h);

		return { test: test, examples: examples };
	}
}

let list: Garden[] = [];
let test: Koan.ProducedStatement<any>;
window.onload = function() {
	let rule = makeNewExample();
	let garden = Koan.makeRandomGarden();
	list = rule.examples;
	test = rule.test;

	function renderList() {
		function makeGarden(g: Garden, i: number) {
			return <GardenDisplay
				garden={g}
				key={i + Koan.gardenToString(g)}
				test={test}
				leftButton='✗'
				rightButton='✎'
				onLeftButtonClicked={() => {
					console.log(list.indexOf(g));
					list.splice(list.indexOf(g), 1);
					renderList();
				}}
				onRightButtonClicked={() => {
					garden = g;
					renderEditor();
				}}
			/>;
		}
		let gardenList = <div>{list.map(makeGarden)}</div>;
		React.render(gardenList, document.getElementById('results'));
	}

	let i = 0;
	function renderEditor() {
		i++;
		let editor = <GardenEditor key={i} test={rule.test} garden={garden} onSaveClicked={(garden) => {
			list.push(garden);
			renderList();
		}} />;
		React.render(editor, document.getElementById('editor'));
	}

	renderList();
	renderEditor();
}

function classNames(nameMap: any): string {
	return Object.keys(nameMap).filter(k => nameMap[k]).join(' ');
}

interface GardenCellProps extends React.Props<{}> {
	color: Gardens.RockColor;
	shape: Gardens.RockShape;
	index: number;

	movable?: boolean;
	onEdit?(newColor: Gardens.RockColor, newShape: Gardens.RockShape): void;
}
interface GardenCellState {
	isDragging?: boolean;
}
class GardenCell extends React.Component<GardenCellProps, GardenCellState> {
	state: GardenCellState = {};
	ignoreNextEdit = false;

	render() {
		var classes = ['cell', 'index_' + this.props.index];

		if (this.state.isDragging) {
			// Render as blank
		} else {
			classes.push(Gardens.RockColor[this.props.color], Gardens.RockShape[this.props.shape]);
		}

		if (this.props.movable) classes.push('movable');
		let events: React.HTMLAttributes = {
			onDragStart: (e) => {
				this.ignoreNextEdit = false;
				e.dataTransfer.dropEffect = 'copyMove';
				e.dataTransfer.effectAllowed = 'move';
				e.dataTransfer.setData('shape', this.props.shape.toString());
				e.dataTransfer.setData('color', this.props.color.toString());

				let drag = document.getElementById(getGardenName(this.props.color, this.props.shape));
				let xfer: any = (e.nativeEvent as DragEvent).dataTransfer;
				xfer.setDragImage(drag, drag.clientWidth * 0.5, drag.clientHeight * 0.5);

				this.setState({ isDragging: true });
			},
			onDragEnter: (e) => {
				e.dataTransfer.dropEffect = 'move';
				e.preventDefault();
			},
			onDragOver: (e) => {
				e.dataTransfer.dropEffect = 'move';
				e.preventDefault();
			},
			onDragEnd: (e) => {
				this.setState({ isDragging: false });
				if (!this.ignoreNextEdit) {
					this.props.onEdit && this.props.onEdit(undefined, undefined);
				}
			},
			draggable: true
		}

		let handleDrop = (event: React.DragEvent) => {
			if(this.props.onEdit) {
				if (this.state.isDragging) {
					// Dragged to self, don't do anything
					this.ignoreNextEdit = true;
				} else {
					let shape: Gardens.RockShape = +event.dataTransfer.getData('shape');
					let color: Gardens.RockColor = +event.dataTransfer.getData('color');
					this.props.onEdit(color, shape);
				}
			}
		}

		return <span className={classes.join(' ')} onDrop={handleDrop} {...this.props.movable ? events : {}} />;
	}
}

interface GardenDisplayProps extends React.Props<GardenDisplay> {
	garden?: Garden;
	test?: Koan.ProducedStatement<any>;

	leftButton?: string;
	rightButton?: string;
	onLeftButtonClicked?(): void;
	onRightButtonClicked?(): void;

	editable?: boolean;
	onChanged?(newGarden: Garden): void;
}
interface GardenDisplayState {
	garden?: Garden;
}
class GardenDisplay extends React.Component<GardenDisplayProps, GardenDisplayState> {
	state = {
		garden: Koan.cloneGarden(this.props.garden)
	};

	leftClicked = () => {
		this.props.onLeftButtonClicked && this.props.onLeftButtonClicked();
	};

	rightClicked = () => {
		this.props.onRightButtonClicked && this.props.onRightButtonClicked();
	};

	render() {
		let g = this.state.garden;
		let pass = (this.props.test && this.props.test.test(this.state.garden));

		let classes = {
			garden: true,
			unknown: pass === undefined,
			pass: pass === Koan.StateTestResult.Pass || pass === Koan.StateTestResult.WeakPass,
			fail: pass === Koan.StateTestResult.Fail,
			editable: this.props.editable
		};

		var children = g.colors.map((_, i) => (
			<GardenCell
				key={i}
				color={g.colors[i]}
				shape={g.shapes[i]}
				index={i}
				movable={this.props.editable}
				onEdit={(newColor, newShape) => {
					if(this.props.editable) {
						let newGarden = Koan.cloneGarden(this.state.garden);
						newGarden.colors[i] = newColor;
						newGarden.shapes[i] = newShape;
						this.setState({ garden: newGarden });
						this.props.onChanged && this.props.onChanged(newGarden);
					}
				}}
			/>));

		return <div className="gardenDisplay">
			<div className={classNames(classes)}>{children}</div>
			<span className="infoRow">
				{this.props.leftButton && <div className="button left" onClick={this.leftClicked}>{this.props.leftButton}</div>}
				<div className={"passfail " + (pass ? 'pass' : 'fail')}>{pass ? '✓' : '🚫'}</div>
				{this.props.rightButton && <div className="button right" onClick={this.rightClicked}>{this.props.rightButton}</div>}
			</span>
		</div>;
	}
}

interface GardenEditorProps extends React.Props<GardenEditor> {
	onSaveClicked?(garden: Garden): void;
	test?: Koan.ProducedStatement<any>;
	garden?: Garden;
}
interface GardenEditorState {
	garden?: Garden;
	pass?: boolean;
}
class GardenEditor extends React.Component<GardenEditorProps, {}> {
	state = { garden: this.props.garden };

	save = () => {
		this.props.onSaveClicked && this.props.onSaveClicked(this.state.garden);
	};

	render() {
		return <div className="editor">
			<GardenDisplay garden={this.state.garden} test={this.props.test} editable onChanged={g => this.setState({ garden: g }) } />
			<StonePalette />
			<div className="button save" onClick={this.save}>{'💾'}</div>
		</div>;
	}
}

class StonePalette extends React.Component<{}, {}> {
	render() {
		let items: JSX.Element[] = [];
		Gardens.RockColors.forEach(color => {
			Gardens.RockShapes.forEach(shape => {
				let name = getGardenName(color, shape);
				let extraProps = { id: name, key: name };
				let index = items.length;
				items.push(<GardenCell
					color={color}
					shape={shape}
					index={index}
					movable
					{...extraProps} />)
			});
		});
		return <div className="palette">{items}</div>;
	}
}

function getGardenName(color: Gardens.RockColor, shape: Gardens.RockShape) {
	return 'draggable.' + Gardens.RockShape[shape] + '.' + Gardens.RockColor[color];
}


