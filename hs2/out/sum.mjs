import {module} from "./sum.wasm.mjs";
import * as sum from "./sum.lib.mjs";
module.then(m => sum.newInstance(m)).then(i => {
try {
i.wasmInstance.exports.hs_init();
i.wasmInstance.exports.sumIgrepCashbook("test", "aaa");
i.wasmInstance.exports.main();
} catch (err) {
console.log(i.stdio.stdout());
throw err;
}
console.log(i.stdio.stdout());
});
