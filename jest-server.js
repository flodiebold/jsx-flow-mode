console.error("NODE_PATH:", process.env.NODE_PATH);
console.error("cwd:", process.cwd());

const { TestRunner, TestWatcher } = require("jest-cli");
const createHasteContext = require("jest-cli/build/lib/createHasteContext");
const { readConfig } = require("jest-config");
const { NullConsole } = require("jest-util");
const Runtime = require("jest-runtime");
const BaseReporter = require("jest-cli/build/reporters/BaseReporter");
const readline = require("readline");

let config;
let hasteMap;
let hasteContext;

const nullConsole = new NullConsole(process.stdout, process.stderr);

const configOverrides = {
    notify: false,
    collectCoverage: false,
    verbose: false
};

class SilentRunner extends TestRunner {
    _setupReporters() {}
}

function start() {
    return readConfig([], process.cwd())
        .then(c => {
            config = c.config;

            hasteMap = Runtime.createHasteMap(Object.assign({}, c.config, configOverrides), {
                console: nullConsole,
                maxWorkers: 4,
                resetCache: false,
                watch: true
            });

            return hasteMap.build();
        })
        .then(builtMap => createHasteContext(config, builtMap))
        .then(context => {
            hasteContext = context;
            const runner = makeRunner();
            return runner;
        });
}

class MyReporter extends BaseReporter {
    onRunStart(config, results, runnerContext, options) {
        const { estimatedTime } = options;
        const data = {
            event: "runStart",
            estimatedTime
        };
        writeData(data);
    }

    onTestStart(config, path, runnerContext) {
        const data = {
            event: "testStart",
            path
        };
        writeData(data);
    }

    onTestResult(config, testResult, results) {
        const data = Object.assign({ event: "testResult" }, testResult);
        writeData(data);
    }

    onRunComplete(config, aggregatedResults, runnerContext) {
        const data = {
            event: "runComplete"
        };
        writeData(data);
    }
}

function makeRunner() {
    const runner = new SilentRunner(hasteContext, config, { maxWorkers: 4 }, () => console.log("startRun"));
    runner.addReporter(new MyReporter());
    return runner;
}

function runTests(runner, files) {
    runner.runTests(files, new TestWatcher({ isWatchMode: true }));
}

const runnerPromise = start();

function writeData(data) {
    process.stdout.write(JSON.stringify(data) + "\n");
}

const commandHandlers = {
    ping() {
        writeData({ event: "pong" });
    },
    runTests({ files }) {
        runnerPromise.then(runner => runTests(runner, files));
    }
};

function dispatchCommand(data) {
    commandHandlers[data.command](data);
}

function readCommands() {
    const rl = readline.createInterface({
        input: process.stdin
    });

    rl.on("line", input => {
        try {
            const data = JSON.parse(input.trim());
            dispatchCommand(data);
        } catch (e) {
            console.error("Command errored:", input, e);
        }
    });
}

readCommands();
