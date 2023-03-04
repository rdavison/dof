@react.component
let make = (~children) => {
  <div className="px-10 min-h-screen bg-slate-400 flex justify-center items-center">
    {children}
  </div>
}
