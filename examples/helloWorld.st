/*
interface Console {
    assert(condition?: boolean, ...data: any[]): void;
    clear(): void;
    count(label?: string): void;
    countReset(label?: string): void;
    debug(...data: any[]): void;
    dir(item?: any, options?: any): void;
    dirxml(...data: any[]): void;
    error(...data: any[]): void;
    group(...data: any[]): void;
    groupCollapsed(...data: any[]): void;
    groupEnd(): void;
    info(...data: any[]): void;
    log(...data: any[]): void;
    table(tabularData?: any, properties?: string[]): void;
    time(label?: string): void;
    timeEnd(label?: string): void;
    timeLog(label?: string, ...data: any[]): void;
    timeStamp(label?: string): void;
    trace(...data: any[]): void;
    warn(...data: any[]): void;
}
*/

/*
Next update :>
extern namespace console {
    fn assert(bool, ...: any);
    fn clear();
    fn count(string?);
    fn countReset(string?);
    fn debug(...: any);
    fn dir(item: any?, options: any?);
    fn dirxml(...: any);
    fn error(....: any);
    fn group(...: any);
    fn groupCollapsed(...: any);
    fn groupEnd();
    fn log(...: any);
    fn table(any, string[]?); TODO: Implement arrays
    fn time(string?);
    fn timeEnd(string?);
    fn timeLog(string?, ...: any);
    fn timeStamp(string?);
    fn trace(...: any);
    fn warn(...: any);
}
*/

fn main() {
    js {
        "console.log(\"Goodbye World!\");\n"
        "console.log(\"Hello World!\");\n"
        "process.exit(69)"
    }
}